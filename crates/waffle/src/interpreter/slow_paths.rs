use super::*;

#[cold]
pub fn slow_acc_field(
    vm: &mut VM,
    object: &Value,
    module: &mut Rooted<Nullable<Module>>,
    name: Symbol,
    fdbk: usize,
) -> Value {
    gc_frame!(vm.gc().roots() => this = *object, object = object.to_object(vm));
    if Object::is(&object, ARRAY_CLASS) && name == vm.id(Id::Length) {
        let weak = vm.gc().weak(*object.structure());
        module.feedback[fdbk] = Feedback::PropertyCache {
            structure: weak,
            offset: u32::MAX,
            mode: GetByIdMode::ArrayLength,
        };
        vm.gc().write_barrier(module.feedback.as_gc());
        return Value::Int(object.indexed().length() as _);
    }
    gc_frame!(vm.gc().roots() => slot = Slot::new());
    let found = object.get_property_slot(vm, name, &mut slot);
    if found
        && slot.is_load_cacheable()
        && Nullable::ptr_eq(
            slot.base
                .map(|x| x.as_dyn().nullable())
                .unwrap_or_else(|| Nullable::NULL),
            object.as_dyn().nullable(),
        )
    {
        let weak = vm.gc().weak(*object.structure());
        module.feedback[fdbk] = Feedback::PropertyCache {
            structure: weak,
            offset: slot.offset(),
            mode: GetByIdMode::Default,
        };
        vm.gc().write_barrier(module.feedback.as_gc());
    } else if found && slot.is_load_cacheable() && slot.base.is_some_and(|x| x.is::<Object>()) {
        if !slot.is_not_found() {
            let weak_structure = vm.gc().weak(*object.structure());
            let weak_object = vm.gc().weak(slot.base().unwrap().downcast().unwrap());
            let weakp = vm.gc().weak(
                *slot
                    .base()
                    .unwrap()
                    .downcast::<Object>()
                    .unwrap()
                    .structure(),
            );
            module.feedback[fdbk] = Feedback::ProtoLoad {
                structure: weak_structure,
                cached_offset: slot.offset(),
                cached_slot: weak_object,
                proto_structure: weakp,
            };
        }
    }

    slot.get(vm, *this)
}

#[cold]
pub fn slow_set_field(
    vm: &mut VM,
    object: &Value,
    module: &mut Rooted<Nullable<Module>>,
    name: Symbol,
    fdbk: usize,
    value: &Value,
) {
    gc_frame!(vm.gc().roots() => _this = *object, object = object.to_object(vm));
    gc_frame!(vm.gc().roots() => slot = Slot::new(), old_structure = *object.structure());

    object.put_slot(vm, name, *value, &mut slot, false);

    if slot.is_put_cacheable() {
        gc_frame!(vm.gc().roots() => new_structure = *object.structure());
        if Gc::ptr_eq(slot.base().unwrap(), object.as_dyn()) {
            if slot.put_result_type() == PutResultType::New {
                if !new_structure.is_unique()
                    && new_structure.previous.as_gc().storage_capacity()
                        == new_structure.storage_capacity()
                {
                    if Gc::ptr_eq(new_structure.previous.as_gc(), *old_structure) {
                        let new_structurew = vm.gc().weak(*new_structure);
                        let old_structure = vm.gc().weak(new_structure.previous.as_gc());
                        let chain = new_structure.prototype_chain(vm, *object);
                        let chain = vm.gc().weak(chain);
                        module.feedback[fdbk] = Feedback::PutNewCache {
                            new_structure: new_structurew,
                            old_structure,
                            offset: slot.offset(),
                            chain,
                        };

                        vm.gc().write_barrier(module.as_gc());
                    }
                }
            } else {
                let weak = vm.gc().weak(*new_structure);
                module.feedback[fdbk] = Feedback::PutReplaceCache {
                    structure: weak,
                    offset: slot.offset(),
                };
                vm.gc().write_barrier(module.as_gc());
            }
        }
    }
}
