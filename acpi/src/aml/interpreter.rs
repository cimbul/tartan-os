//! AML interpreter

use alloc::string::String;
use alloc::boxed::Box;
use alloc::vec::Vec;
use alloc::collections::BTreeMap;
use crate::aml::term;
use crate::aml::name;
use crate::aml::data;
use crate::aml::misc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Uninitialized,
    Integer(u64),
    String(String),
    Buffer(Box<[u8]>),
    Package(Box<[Object]>),
    // TODO: FieldUnit?
}

type NameSpaceEntries = BTreeMap<name::NameSeg, Object>;

pub trait ExternalState {

}

pub enum ControlFlow<'a> {
    Normal,
    Continue,
    Break,
    Return(&'a Object),
}


pub struct Interpreter<E: ExternalState> {
    external_state: E,
    root_entries: NameSpaceEntries,
    current_scope: Vec<name::NameSeg>,
    current_args: [Option<Object>; 7],
    current_locals: [Option<Object>; 8],
}

impl<E: ExternalState> Interpreter<E> {
    pub fn new(external_state: E) -> Interpreter<E> {
        return Interpreter {
            external_state: external_state,
            root_entries: NameSpaceEntries::new(),
            current_scope: Vec::new(),
            current_args: [None, None, None, None, None, None, None],
            current_locals: [None, None, None, None, None, None, None, None],
        };
    }

    pub fn interpret_term(&mut self, term_obj: &term::TermObject) -> ControlFlow {
        match term_obj {
            term::TermObject::Modifier(ref m) => self.interpret_modifier(m),
            term::TermObject::Named(ref n) => self.interpret_named(n),
            term::TermObject::Statement(ref s) => self.interpret_statement(s),
            term::TermObject::Expression(ref e) => {
                self.evaluate_expression(e);
                ControlFlow::Normal
            }
        }
    }

    pub fn evaluate_term_arg(&mut self, term_arg: &term::TermArg) -> &Object {
        match term_arg {
            term::TermArg::Expression(ref e) => self.evaluate_expression(e),
            term::TermArg::Data(ref d) => self.evaluate_data_object(d),
            term::TermArg::Arg(ref a) => self.evaluate_arg(a),
            term::TermArg::Local(ref l) => self.evaluate_local(l),
            term::TermArg::Name(ref n) => self.evaluate_name(n),
        }
    }

    fn interpret_modifier(&mut self, _: &term::NameSpaceModifier) -> ControlFlow {
        todo!();
    }

    fn interpret_named(&mut self, _: &term::NamedObject) -> ControlFlow {
        todo!();
    }

    fn interpret_statement(&mut self, _: &term::StatementOpcode) -> ControlFlow {
        todo!();
    }

    fn evaluate_expression(&mut self, _: &term::ExpressionOpcode) -> &Object {
        todo!();
    }

    fn evaluate_data_object(&mut self, _: &data::DataObject) -> &Object {
        todo!();
    }

    fn evaluate_buffer(&mut self, _: &data::Buffer) -> &Object {
        todo!();
    }

    fn evaluate_package(&mut self, _: &data::Package) -> &Object {
        todo!();
    }

    fn evaluate_var_package(&mut self, _: &data::VarPackage) -> &Object {
        todo!();
    }

    fn evaluate_arg(&mut self, arg: &misc::ArgObject) -> &Object {
        let index = match arg {
            misc::ArgObject::Arg0 => 0,
            misc::ArgObject::Arg1 => 1,
            misc::ArgObject::Arg2 => 2,
            misc::ArgObject::Arg3 => 3,
            misc::ArgObject::Arg4 => 4,
            misc::ArgObject::Arg5 => 5,
            misc::ArgObject::Arg6 => 6,
        };
        return self.current_args[index].as_ref().unwrap_or(&Object::Uninitialized);
    }

    fn evaluate_local(&mut self, arg: &misc::LocalObject) -> &Object {
        let index = match arg {
            misc::LocalObject::Local0 => 0,
            misc::LocalObject::Local1 => 1,
            misc::LocalObject::Local2 => 2,
            misc::LocalObject::Local3 => 3,
            misc::LocalObject::Local4 => 4,
            misc::LocalObject::Local5 => 5,
            misc::LocalObject::Local6 => 6,
            misc::LocalObject::Local7 => 7,
        };
        return self.current_locals[index].as_ref().unwrap_or(&Object::Uninitialized);
    }

    fn evaluate_name(&mut self, _: &name::NameString) -> &Object {
        todo!();
    }
}
