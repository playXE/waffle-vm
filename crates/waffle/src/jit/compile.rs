use std::collections::BTreeMap;

use codegen::isa;
use cranelift::codegen::ir::StackSlot;
use cranelift::prelude::{codegen::isa::*, Signature};
use cranelift::prelude::{MemFlags, StackSlotData};
use cranelift::{
    codegen::ir,
    frontend::Variable,
    prelude::{
        codegen::settings::{self, Configurable},
        types, AbiParam,
    },
};
use cranelift::{
    codegen::ir::{FuncRef, InstBuilder},
    frontend::FunctionBuilder,
    prelude::{IntCC, JumpTableData},
};
use cranelift::{
    codegen::{self},
    frontend::FunctionBuilderContext,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use memoffset::offset_of;
use target_lexicon::Triple;

use crate::value::Value;
use crate::vm::VM;

use super::stackir::Ins;
fn get_isa() -> Box<dyn TargetIsa + 'static> {
    let mut flags_builder = codegen::settings::builder();
    flags_builder.set("opt_level", "speed").unwrap();
    flags_builder.set("is_pic", "false").unwrap();
    codegen::isa::lookup(Triple::host())
        .unwrap()
        .finish(settings::Flags::new(flags_builder))
        .unwrap()
}

const TRIPLE: Triple = Triple::host();

pub fn calling_convention() -> isa::CallConv {
    isa::CallConv::triple_default(&TRIPLE)
}

pub struct BasicBlock {
    pub low: usize,
    pub high: usize,
    pub max: usize,
    pub block: ir::Block,
    pub stack_state: Box<[ir::Value]>,
}

impl BasicBlock {
    pub fn new(loc: usize, block: ir::Block) -> Self {
        Self {
            block,
            low: loc,
            high: loc,
            max: usize::MAX,

            stack_state: vec![].into_boxed_slice(),
        }
    }
}

pub struct Compiler<'a> {
    builder: FunctionBuilder<'a>,
    operand_stack: Vec<ir::Value>,
    vm: ir::Value,
    instr_index: usize,
    ungenerated: Vec<BasicBlock>,
    current_block: BasicBlock,
    blocks: BTreeMap<u32, ir::Block>,
    end_of_basic_block: bool,
    fallthrough: bool,
    runoff: usize,
    acc: Variable,
    code: &'a [super::stackir::Ins],
}

impl Compiler<'_> {
    pub fn save(&mut self) {
        // let sp = vm.sp;
        let sp = self.builder.ins().load(
            types::I64,
            MemFlags::new(),
            self.vm,
            offset_of!(VM, sp) as i32,
        );

        for (i, value) in self.operand_stack.iter().rev().copied().enumerate() {
            // sp[-i] = value;
            self.builder
                .ins()
                .store(MemFlags::new(), value, sp, -(i as i32 * 8));
        }
        let imm = self
            .builder
            .ins()
            .iconst(types::I64, self.operand_stack.len() as i64 * 8);
        // vm.sp = sp - stack_size * 8;

        let new_sp = self.builder.ins().isub(sp, imm);

        self.builder
            .ins()
            .store(MemFlags::new(), new_sp, self.vm, offset_of!(VM, sp) as i32);

        let acc = self.builder.use_var(self.acc);
        self.builder
            .ins()
            .store(MemFlags::new(), acc, self.vm, offset_of!(VM, acc) as i32);
    }

    pub fn restore(&mut self) {
        // let sp = vm.sp;
        let sp = self.builder.ins().load(
            types::I64,
            MemFlags::new(),
            self.vm,
            offset_of!(VM, sp) as i32,
        );

        for i in 0..self.operand_stack.len() {
            let value_ptr = self.builder.ins().iadd_imm(sp, i as i64 * 8);
            let value = self
                .builder
                .ins()
                .load(types::I64, MemFlags::new(), value_ptr, 0);
            self.operand_stack[i] = value;
        }

        let new_sp = self
            .builder
            .ins()
            .iadd_imm(sp, self.operand_stack.len() as i64 * 8);
        self.builder
            .ins()
            .store(MemFlags::new(), new_sp, self.vm, offset_of!(VM, sp) as i32);
        let acc = self.builder.ins().load(
            types::I64,
            MemFlags::new(),
            self.vm,
            offset_of!(VM, acc) as i32,
        );

        self.builder.def_var(self.acc, acc);
    }

    fn get_or_create_block(&mut self, offset: u32) -> ir::Block {
        self.end_of_basic_block = true;
        *self.blocks.entry(offset).or_insert_with(|| {
            let block = self.builder.create_block();
            for _ in 0..self.operand_stack.len() {
                self.builder.append_block_param(block, types::I32);
            }
            let mut bb = BasicBlock::new(offset as _, block);

            bb.stack_state = self.operand_stack.clone().into_boxed_slice();
            self.ungenerated.push(bb);
            block
        })
    }

    fn generate_from(&mut self, from: usize) {
        self.builder.switch_to_block(self.current_block.block);

        let mut index = from;
        self.end_of_basic_block = false;
        self.fallthrough = false;

        loop {
            self.current_block.high = from;
            self.instr_index = index;

            let code = self.code[index];
            index += 1;
            let mut s = None;
            match code {
                Ins::AccInt(x) => {
                    let val = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::new(x).raw() as i64);
                    self.builder.def_var(self.acc, val);
                }

                Ins::Push => {
                    let acc = self.builder.use_var(self.acc);
                    let c = self.builder.ins().copy(acc);
                    self.operand_stack.push(c);
                }
                Ins::AccStack(x) => {
                    let val = self.operand_stack[x as usize];
                    self.builder.def_var(self.acc, val);
                }
                Ins::SetStack(x) => {
                    let val = self.builder.use_var(self.acc);
                    self.operand_stack[x as usize] = val;
                }
                Ins::Jump(off) => {
                    
                }
                _ => todo!(),
            }
        }
    }
}
