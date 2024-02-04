use std::collections::HashMap;
//use std::borrow::Borrow;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue};
//use inkwell::FloatPredicate;
use crate::parser::{Expr, Function, Prototype};


pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<FloatValue<'ctx>, &'static str> {
        match *expr {
            Expr::Number(nb) => Ok(self.context.f64_type().const_float(nb)),
            Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
                Some(var) => Ok(self.builder.build_load( self.context.f64_type(),*var, name.as_str()).into_float_value()), // doubt type
                None => Err("()")
            },
            Expr::Binary(op, ref left,  ref right) => {
                if op == '=' {
                    // let var_name = match *left.borrow() {
                    //     Expr::Variable(ref var_name) => var_name,
                    //     _ => {return Err("expected variable");}
                    // };

                    let var_val = self.compile_expr(right)?;
                    //let var = self.variables.get(var_name.as_str()).ok_or("undefined variable");
                    Ok(var_val)
                } else {
                    let lhs = self.compile_expr(left)?;
                    let rhs = self.compile_expr(right)?;
                    match op {
                        '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
                        '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
                        '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
                        '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")), 
                        custom => {
                            let mut name = String::from("binary");
                            name.push(custom);
                            match self.get_function(name.as_str()) {
                                Some(fun) => {
                                    match self.builder
                                        .build_call(fun, &[lhs.into(), rhs.into()], "tmpbin")
                                        .try_as_basic_value()
                                        .left() 
                                    {
                                        Some(value) => Ok(value.into_float_value()),
                                        None => Err("invalid call produced")    
                                    }
                                }
                                None => Err("undefined binary operator")
                            }
                        }   
                    }
                }


            },
            Expr::Call(ref fn_name, ref args) => match self.get_function(fn_name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(args.len());
                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<BasicMetadataValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
                    match self.builder
                        .build_call(fun, argsv.as_slice(), "tmp") 
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err("Invalid call produced")
                    }
                },
                None => Err("unknown function")
            }
        }
    }

    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }

    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.proto;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(proto.args[i].clone(), alloca);
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err("Invalid generated function.")
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut compiler = Compiler {
            context,
            builder,
            fpm: pass_manager,
            module,
            function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}
