#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(dead_code)]

use std::borrow::BorrowMut;
use std::env;
use std::io::{BufRead};
use std::num::ParseIntError;
use std::ops::Index;
use std::str::FromStr;
use std::vec::Vec;
use regex::{Match, Regex, RegexBuilder};

////////////////////////////////////////////// Some Definitions /////////////////////////////////
#[derive(Debug)]
enum InstructionItem
{
    Inst_dp(u32),
    Inst_add,
    Inst_and,
    Inst_break,
    Inst_itof,
    Inst_loop,
    Inst_min,
    Inst_max,
    Inst_mov,
    Inst_movc,
    Inst_lt,
    Inst_ne,
    Inst_div,
    Inst_mad,
    Inst_mul,
    Inst_exp,
    Inst_sample,
    Inst_sample_b,
    Inst_sample_c,
    Inst_sample_l,
    Inst_sample_d,
    Inst_rsq,
    Inst_sqrt,
}

impl core::str::FromStr for InstructionItem
{
    type Err = ();
    fn from_str(s:&str) ->Result<InstructionItem, Self::Err>
    {
         match s
        {
            "dp"    => {Ok(InstructionItem::Inst_dp(0))},
            "add"   => {Ok(InstructionItem::Inst_add)},
            "and"   => {Ok(InstructionItem::Inst_and)},
            "break" => {Ok(InstructionItem::Inst_break)},
            "itof"  => {Ok(InstructionItem::Inst_itof)},
            "loop"  => {Ok(InstructionItem::Inst_loop)},
            "min"   => {Ok(InstructionItem::Inst_min)},
            "max"   => {Ok(InstructionItem::Inst_max)},
            "mov"   => {Ok(InstructionItem::Inst_mov)},
            "movc"   => {Ok(InstructionItem::Inst_movc)},
            "lt"   => {Ok(InstructionItem::Inst_lt)},
            "div"   => {Ok(InstructionItem::Inst_div)},
            "ne"   => {Ok(InstructionItem::Inst_ne)},
            "mad"   => {Ok(InstructionItem::Inst_mad)},
            "mul"   => {Ok(InstructionItem::Inst_mul)},
            "exp"   => {Ok(InstructionItem::Inst_exp)},
            "sample_c"  |
            "sample"    |
            "sample_b"  |
            "sample_l"  |
            "sample_d"  =>{Ok(InstructionItem::Inst_sample)},
            "rsq"   => {Ok(InstructionItem::Inst_rsq)},
            "sqrt"  => { Ok(InstructionItem::Inst_sqrt) },
                _   =>{panic!("unsuppored inst {}", s)}
        }
    }
}

#[derive(Debug)]
struct OpCode
{
    pub Name        :   std::string::String,
    pub Dim         :   Option<u32>,
    pub Posts       :   Vec<std::string::String>,
    pub InstItem    :   InstructionItem
}

impl OpCode
{
    fn new(name:&str, posts:Vec<String>, dim:Option<u32>)->OpCode
    {
        OpCode{
            Name        : name.to_string(),
            Dim         : dim,
            Posts       : posts,
            InstItem    : name.parse::<InstructionItem>().unwrap()
        }
    }
}

impl std::string::ToString for OpCode
{
    fn to_string(&self) -> String {
        format!("{}{}", &self.Name, match self.Dim
        {
            Some(d) => {d.to_string()}
            _=> {"".to_string()}
        })
    }
}
#[derive(Debug,Clone, PartialEq)]
enum Swizzle
{
    x = 0,
    y = 1,
    z = 2,
    w = 3,
}

impl std::convert::From<&Swizzle> for usize
{
    fn from(s:&Swizzle) -> usize {
        match s
        {
            Swizzle::x => {0 as usize},
            Swizzle::y => {1 as usize},
            Swizzle::z => {2 as usize},
            Swizzle::w => {3 as usize},
        }
    }
}

#[derive(Debug,Clone)]
struct SwizzlesMask(Vec<Option<Swizzle>>);

impl SwizzlesMask
{
    fn new()->SwizzlesMask
    {
        SwizzlesMask(vec![
            None,
            None,
            None,
            None]
        )
    }

    fn one_mask(s: &Swizzle) ->SwizzlesMask
    {
        SwizzlesMask(vec![
            Some(s.clone()),
            None,
            None,
            None]
        )
    }

    fn n_mask(n:i32) ->SwizzlesMask
    {
        let t = vec![
            Some(Swizzle::x),
            Some(Swizzle::y),
            Some(Swizzle::z),
            Some(Swizzle::w)
        ];

       let mut o= SwizzlesMask(vec![
            None,
            None,
            None,
            None]
        );

        for i in 0..n
        {
            o.0[i as usize] = t[i as usize].clone();
        }
        o
    }

    fn all_mask()->SwizzlesMask
    {
        SwizzlesMask(vec![
            Some(Swizzle::x),
            Some(Swizzle::y),
            Some(Swizzle::z),
            Some(Swizzle::w)]
        )
    }

    fn get_effective_len(&self)->i32
    {
        let mut x = 0;
        for it in &self.0
        {
            if it.is_some()
            {
                x += 1;
            }
        }
        return x;
    }
}

#[derive(Debug)]
struct VarOperand
{
    pub Neg:        bool,
    pub Name:       std::string::String,
    pub NameDim:    Option<u32>,
    pub Dim:        Option<u32>,
    pub Swizzles:   SwizzlesMask,
    pub IsSampler:  bool,
    pub IsTexture:  bool,
}

impl  core::str::FromStr for SwizzlesMask
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut sw = SwizzlesMask::new();
        let mut index:usize = 0;
        for c in s.chars().into_iter()
        {
            match c
            {
                'x' => {sw.0[index] = Some(Swizzle::x);},
                'y' => {sw.0[index] = Some(Swizzle::y);},
                'z' => {sw.0[index] = Some(Swizzle::z);},
                'w' => {sw.0[index] = Some(Swizzle::w);},
                _ => { panic!("unsupported swizzle mask {}", c)}
            }
            index += 1;
        }
        Ok(sw)
    }
}

impl std::string::ToString for SwizzlesMask
{
    fn to_string(&self) -> String {
        let xyzw = vec!['x','y','z','w'];
        let mut ret = "".to_string();
        for s in self.0.iter()
        {
            if s.is_none()
            {
                break;
            }
            ret.push(xyzw[usize::from(s.as_ref().unwrap())]);
        }
        return ret;
    }
}

impl std::string::ToString for VarOperand
{
    fn to_string(&self) -> String {
       format!("{}{}{}.{}",
               &self.Name,
               self.NameDim.as_ref().unwrap().to_string(),
               match self.Dim
               {
                   Some(d) => {
                       format!("[{}]", d.to_string())
                   },
                    _ => {"".to_string()}
               },
           self.Swizzles.to_string())
    }
}

impl VarOperand
{
    fn new(neg:bool, name:&str, nameDim:Option<u32>, dim:Option<u32>, swizzles:&str)->VarOperand
    {
        VarOperand
        {
            Neg         : neg,
            Name        : name.to_string(),
            NameDim     : nameDim,
            Dim         : dim,
            Swizzles    : swizzles.parse::<SwizzlesMask>().unwrap(),
            IsSampler   : if name == "s" && nameDim.is_some() && swizzles=="" {true} else {false},
            IsTexture   : if name == "t" && nameDim.is_some() {true} else {false}
        }
    }

    fn full_name(&self)->String
    {
        let mut part = format!("{}", self.Name);
        if let Some(dim) = self.NameDim {
            part.push_str(&dim.to_string());
        };
        if let Some(dim) = self.Dim {
            part.push_str(&format!("[{}]", dim));
        }
        return part;
    }

    fn var_name(&self)->String
    {
        let mut part = format!("{}", self.Name);
        if let Some(dim) = self.NameDim {
            part.push_str(&dim.to_string());
        };
        part
    }
    fn dim_name(&self)->String
    {
        match self.Dim
        {
            Some(d) => format!("[{}]",d.to_string()),
            _ => "".to_string()
        }
    }
    fn dim_swizzle_name(&self)->String
    {
        let mut part = "".to_string();
        if let Some(dim) = self.Dim {
            part.push_str(&format!("[{}]", dim));
        }
        let sStr = self.Swizzles.to_string();
        if sStr == ""
        {
            return part;
        }
        part.push_str(".");
        part.push_str(&sStr);
        return part;
    }

    fn swizzles_all_same(&self)->bool
    {
        if self.Swizzles.to_string().eq("xxxx") ||
            self.Swizzles.to_string().eq("yyyy") ||
            self.Swizzles.to_string().eq("zzzz") ||
            self.Swizzles.to_string().eq("wwww")
        {
            return true;
        }
        false
    }
    fn get_swizzles(&self, mask:&SwizzlesMask)->SwizzlesMask
    {
        //println!("{:?}", mask);
        if self.IsSampler
        {
            return self.Swizzles.clone();
        }
        if mask.get_effective_len() == 1
        {
            return self.Swizzles.clone();
        }

        if self.swizzles_all_same()
        {
            return SwizzlesMask::one_mask(self.Swizzles.0[0].as_ref().unwrap())
        }

        let mut output = SwizzlesMask::new();
        let mut index:usize = 0;
        for it in mask.0.iter()
        {
            if it.is_none()
            {
                return output;
            }
            if let Some(s) = it
            {
                output.0[index] = Some(self.Swizzles.0[s.clone() as usize].as_ref().unwrap().clone());
                index += 1;
            }
        }
        return output;
    }
}

#[derive(Debug,Clone)]
enum Number
{
    Int(i32),
    Float(f32),
    Double(f64)
}
impl std::string::ToString for Number
{
    fn to_string(&self) -> String {
        match self
        {
            Number::Int(i)      =>{ i.to_string()},
            Number::Float(f)    =>{ f.to_string()},
            Number::Double(d)   =>{ d.to_string()}
        }
    }
}
impl From<f32> for Number
{
    fn from(v: f32) -> Self {
        Number::Float(v)
    }
}

impl From<f64> for Number
{
    fn from(v: f64) -> Self {
        Number::Double(v)
    }
}
impl From<i32> for Number
{
    fn from(v: i32) -> Self {
        Number::Int(v)
    }
}

#[derive(Debug)]
struct ConstantInitializer
{
    pub Values:std::vec::Vec<Number>
}

impl ConstantInitializer
{
    fn new(v:std::vec::Vec<Number>)->ConstantInitializer
    {
        ConstantInitializer {
            Values: v
        }
    }
}

#[derive(Debug)]
struct ConstantOperand
{
    pub Name            : std::string::String,
    pub Initializer     : ConstantInitializer,
    pub OutputLen       : Option<i32>
}

impl ConstantOperand
{
    fn new(name:&str, initializer:ConstantInitializer)->ConstantOperand
    {
        ConstantOperand
        {
            Name            : name.to_string(),
            Initializer     : initializer,
            OutputLen       : None
        }
    }

    fn output(&self, mask:&SwizzlesMask)->String
    {
        let len = mask.get_effective_len();
        let init :Vec<Number>= self.Initializer.Values[0..len as usize].to_vec();
        ConstantInitializer::new(init).to_string()
    }
}
impl std::string::ToString for ConstantOperand
{
    fn to_string(&self) -> String {
       self.Initializer.to_string()
    }
}
impl std::string::ToString for ConstantInitializer
{
    fn to_string(&self) -> String {
        let mut len = self.Values.len();
        match len
        {
            1 => { format!("float({:.6})", self.Values[0].to_string())},
            2 => { format!("float2({:.6},{:.6})", self.Values[0].to_string(),self.Values[1].to_string())},
            3 => { format!("float3({:.6},{:.6},{:.6})", self.Values[0].to_string(),self.Values[1].to_string(),self.Values[2].to_string())},
            4 => { format!("float4({:.6},{:.6},{:.6},{:.6})", self.Values[0].to_string(),self.Values[1].to_string(),self.Values[2].to_string(),self.Values[3].to_string())},
            _ => {
                panic!("unsupported lenght {}", len);
            }
        }
    }
}
impl core::str::FromStr for ConstantInitializer
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splitValues:Vec<&str> = s.split(',').collect();
        let mut v:Vec<Number> = Vec::new();
        for vStr in splitValues
        {
            let trimedStr = vStr.trim();
            //println!("{}", trimedStr);
            if let Ok(value) = trimedStr.parse::<f32>()
            {
                v.push(Number::from(value))
            }
            else if let Ok(value) = trimedStr.parse::<i32>()
            {
                v.push(Number::from(value))
            }
            else
            {
                panic!("parse number failed {}", trimedStr);
            }
        }
        Ok(ConstantInitializer::new(v))
    }
}

#[derive(Debug)]
enum InstructionComponent
{
    OpCode(OpCode),
    VarOperand(VarOperand),
    ConstantOperand(ConstantOperand)
}

impl std::string::ToString for InstructionComponent
{
    fn to_string(&self) -> String {
        match self
        {
            InstructionComponent::OpCode(i) => {
                i.to_string()
            },
            InstructionComponent::VarOperand(v) =>
            {
                v.to_string()
            },
            InstructionComponent::ConstantOperand(c)=>
            {
                c.to_string()
            }
        }
    }
}
#[derive(Debug)]
struct Instruction(std::vec::Vec<InstructionComponent>);

/////////////////////////////////// Lexer ////////////////////////////////////////////////////////
fn DXBCLexer(inputline:&str)->Option<Instruction>
{
    let line = inputline;
    let mut inst = Instruction{ 0: vec![] };
    let pattern = RegexBuilder::new(
        r#"(?x)
                (
                    [-]?[_A-Za-z]+[0-9]*
                )
                [\t\ ]+
                (?:
                    [\t\ ]*
                    (
                        (
                            [-]?[_A-Za-z]+[0-9]*               # operand name
                            (?:
                                \[[0-9]+\]                      # dim
                            )?
                            (?:
                                \.                              # swizzer point
                                [xyzw]{1,5}                     # xyzw
                            )?
                        )
                        |
                        ([-]?
                            [\|]
                                (                               #
                                    [_A-Za-z]+[0-9]*       # operand name
                                    (?:
                                        \[[0-9]+\]              # dim
                                    )?
                                    (?:
                                        \.                      # swizzer point
                                        [xyzw]{1,5}             # xyzw
                                    )
                                )
                            [\|]
                        )
                        |
                        (
                            [-]?[_A-Za-z]+[0-9]*               # constant operand name
                            \(
                                [+-]?[0-9]+
                                (?:
                                    \.[0-9]+
                                )?
                                (?:
                                    [\t\ ]*
                                    ,
                                    [\t\ ]*
                                    [-]?[0-9]+
                                    (?:
                                        \.[0-9]+
                                    )?
                                    [\t\ ]*
                                )*
                            \)
                        )
                    )
                    [\t\ ]*
                    (
                        ,
                        |
                        [\n]
                    )
                )+
            "#,
    ).nest_limit(30).build();
    if pattern.is_err()
    {
        println!("Pattern error {}", pattern.as_ref().err().unwrap());
        return None;
    }
    //println!("{}", line);
    let pattern  = pattern.unwrap();
    let m = pattern.find(line);
    if m.is_none()
    {
        println!("not found {}", line);
        return Option::None;
    }
    if m.as_ref().unwrap().end() != line.as_bytes().len()
    {
        println!("Pattern error {:?} {}", m, line.as_bytes().len());
        return  Option::None;
    }
    // for x in pattern.captures_iter(line)
    // {
    //     println!("{:?}", x);
    // }
    let caps = pattern.captures(line).unwrap();
    let opcodePattern = Regex::new(
        r#"(?x)
               (?P<OpCode>
                    [A-Za-z]+
               )
               (?P<Dim>
                    [0-9]+
               )?
               (?P<Posts>
                    [_A-Za-z]+
               )?
               [\t\ ]+
            "#).unwrap();

    let varOperandPattern = Regex::new(
        r#"(?x)
            [\t\ ]*
            (?P<Neg> -)?
            (?P<PreAbs>\|)?
            (?P<VarName>
                [_A-Za-z]+             # operand name
            )
            (?P<VarNameDim>
                [0-9]+                      # operand name dim
            )?
            (?:
                \[
                    (?P<VarDim>
                        [0-9]+              # dim
                    )
                \]
            )?
            (?:
                \.                              # swizzer point
                (?P<VarSwizzles>
                    [xyzw]{1,5}
                )
            )?
            (?P<PostAbs>\|)?
            [\t\ ]*
            (
                ,
                |
                [\n]
            )
        "#,
    ).unwrap();
    let constOperandPattern = Regex::new(
        r#"(?x)
                    [\t\ ]*
                    (?P<ConstName>
                        [_A-Za-z]+             # operand name
                    )
                    (?P<ConstNameDim>
                        [0-9]+                      # operand name dim
                    )?
                    \(
                        (?P<ConstInitList>
                            [-]?[0-9]+
                            (?:
                                \.[0-9]+
                            )?
                            (?:
                                [\t\ ]*
                                ,
                                [\t\ ]*
                                [-]?[0-9]+
                                (?:
                                    \.[0-9]+
                                )?
                                [\t\ ]*
                            )*
                        )
                    \)
                    [\t\ ]*
                    (
                        ,
                        |
                        [\n]
                    )
            "#).unwrap();
    let fnd = opcodePattern.find(line).unwrap();

    //println!("{:?}", fnd);
    //println!("{}", &line[fnd.start()..fnd.end()]);
   
    let opcaps = opcodePattern.captures(&line[fnd.start()..fnd.end()]).unwrap();
    let mut name = &opcaps["OpCode"];
    let mut posts:Vec<String> = Vec::new();
    // if name.ends_with("_sat")
    // {
    //     post = Some("_sat".to_string());
    //     name = name.trim_end_matches("_sat");
    // }

    if let Some(postsMatch) = opcaps.name("Posts")
    {
        let pstr = postsMatch.as_str();
        let splits = pstr.trim_start_matches("_").split("_");
        for s in splits.into_iter()
        {
            posts.push(s.to_string());
        }
        println!("{:?}", posts);
    }
    let opcode = OpCode::new(
        name,
        posts,
        match opcaps.name("Dim")
        {
            Some(m) => Some(m.as_str().parse::<u32>().unwrap()),
            _ => None
        }
    );
    //println!("{:?}", opcode);
    inst.0.push(InstructionComponent::OpCode(opcode));
    
    let mut start = fnd.end();
    let mut end = line.as_bytes().len();
    //println!("{}", &line[start..end]);
    while start < end
    {
        let fnd = varOperandPattern.find(&line[start..end]);
        if fnd.is_some() && fnd.as_ref().unwrap().start() == 0
        {
            //println!("{:?}", fnd);
            let caps = varOperandPattern.captures(&line[start..end]).unwrap();
            let mut varName = "";
            let mut varNameDim:Option<u32> = None;
            let mut varDim:Option<u32> = None;
            let mut varSwizzles = "";
            let mut neg = false;
            if let Some(negStr) = caps.name("Neg")
            {
                if (negStr.as_str() == "-")
                {
                    neg = true;
                }
            }
            if let Some(m) = caps.name("VarName")
            {
                varName = m.as_str();
            };
            if let Some(m) = caps.name("VarNameDim")
            {
                varNameDim = Some(m.as_str().parse().unwrap());
            };
            if let Some(m) = caps.name("VarDim")
            {
                varDim = Some(m.as_str().parse::<u32>().unwrap());
            };
            if let Some(m) = caps.name("VarSwizzles")
            {
                varSwizzles = m.as_str();
            };
            let varOperand = VarOperand::new(neg,varName, varNameDim, varDim, varSwizzles);
            //println!("{:?}", varOperand);
            start = start + fnd.as_ref().unwrap().end();
            inst.0.push(InstructionComponent::VarOperand(varOperand));
        }

        let fnd = constOperandPattern.find(&line[start..end]);
        if fnd.is_some() && fnd.as_ref().unwrap().start() == 0
        {
            let caps = constOperandPattern.captures(&line[start..end]).unwrap();
            let mut constName = "";
            let mut constInitializer = ConstantInitializer { Values: vec![] };

            if let Some(m) = caps.name("ConstName")
            {
                constName = m.as_str();
            };
            if let Some(m) = caps.name("ConstInitList")
            {
               constInitializer = m.as_str().parse::<ConstantInitializer>().unwrap();
            };
            let constOperand = ConstantOperand::new(constName, constInitializer);
            //println!("{:?}", constOperand);
            start = start + fnd.as_ref().unwrap().end();
            inst.0.push(InstructionComponent::ConstantOperand(constOperand));
        }
        //println!("{} {}", start, end);
    }
    //println!("{:?}", inst);
    if inst.0.len() > 1
    {
        return Some(inst);
    }
    return None;
}

////////////////////////////////////////// Parser //////////////////////////////////////////
#[derive(Debug)]
struct SymbolTable
{
    pub Symbols: std::collections::HashMap<String, String>,
    pub SymCounter: u32,
}

impl SymbolTable
{
    fn new()->SymbolTable
    {
        SymbolTable
        {
            Symbols: std::collections::HashMap::new(),
            SymCounter:10
        }
    }

    fn get_symbol(&mut self, input:&str)->&str
    {
        let ret = self.Symbols.get(input);
        if ret.is_none()
        {
            let sym = format!("local_{}", self.SymCounter);
            self.SymCounter += 1;
            self.Symbols.insert(input.to_string(), sym);
        }
        let ret = self.Symbols.get(input);
        ret.unwrap()
    }
}
struct DXBCParser
{
    pub Insts:std::vec::Vec<Instruction>,
}

impl DXBCParser
{
    fn new() -> DXBCParser
    {
        DXBCParser
        {
            Insts: vec![],
        }
    }

    fn add(&mut self, inst: Instruction)
    {
        self.Insts.push(inst);
    }

    fn parse(&self, symTable: &mut SymbolTable)
    {
        for it in self.Insts.iter()
        {
            self._parse(symTable, it);
        }
    }

    fn _parse(&self, symTable: &mut SymbolTable, inst: &Instruction) -> bool
    {
        match inst.0.get(0).unwrap()
        {
            InstructionComponent::OpCode(_OpInst) =>
                {
                    match _OpInst.InstItem
                    {
                        InstructionItem::Inst_add =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("{} + {}", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_mul =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("{} * {}", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_mad =>
                            {
                                self.parse_3_operands(symTable, inst, &|operand0: &str, operand1: &str, operand2: &str| -> String
                                    {
                                        format!("{} * {} + {}", operand0, operand1, operand2)
                                    })
                            },
                        InstructionItem::Inst_and =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("{} & {}", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_min =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("min( {} , {} )", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_max =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("max( {} , {} )", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_itof =>
                            {
                                self.parse_itof(symTable, inst)
                            },
                        InstructionItem::Inst_mov =>
                            {
                                self.parse_1_operand(symTable, inst, &|operand0: &str| -> String
                                    {
                                        format!("{}", operand0)
                                    })
                            },
                        InstructionItem::Inst_movc =>
                            {
                                self.parse_3_operands(symTable, inst, &|operand0: &str, operand1: &str,operand2: &str| -> String
                                    {
                                        format!("{} ? {} : {}", operand0, operand1, operand2)
                                    })
                            },
                        InstructionItem::Inst_lt =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("{} < {}", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_ne =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("{} != {}", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_div =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("{} / {}", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_loop =>
                            {
                                self.parse_loop(symTable, inst)
                            },
                        InstructionItem::Inst_dp(i) =>
                            {
                                self.parse_2_operands(symTable, inst, &|operand0: &str, operand1: &str| -> String
                                    {
                                        format!("dot( {} , {} )", operand0, operand1)
                                    })
                            },
                        InstructionItem::Inst_exp =>
                            {
                                self.parse_1_operand(symTable, inst, &|operand0: &str| -> String
                                    {
                                        format!("exp( {} )", operand0)
                                    })
                            },
                        InstructionItem::Inst_rsq =>
                            {
                                self.parse_1_operand(symTable, inst, &|operand0: &str| -> String
                                    {
                                        format!("1.0 / sqrt( {} )", operand0)
                                    })
                            },
                        InstructionItem::Inst_sqrt =>
                            {
                                self.parse_1_operand(symTable, inst, &|operand0: &str| -> String
                                    {
                                        format!("sqrt( {} )", operand0)
                                    })
                            },
                        InstructionItem::Inst_sample =>
                            {
                                self.parse_3_operands(symTable, inst, &|operand0: &str,operand1: &str,operand2: &str| -> String
                                    {
                                        format!("sample( {} , {} )", operand0,operand1)
                                    })
                            },
                        _ =>
                            {
                                panic!("unspported instruction {:?}", _OpInst.InstItem);
                            }
                    }
                }
            _ => { false }
        }
    }

    fn parse_sample(&self, symTable: &mut SymbolTable, inst: &Instruction) -> bool
    {
        return true;
    }

    fn parse_loop(&self, symTable: &mut SymbolTable, inst: &Instruction) -> bool
    {
        return true;
    }

    fn parse_itof(&self, symTable: &mut SymbolTable, inst: &Instruction) -> bool
    {
        return true;
    }

    fn parse_1_operand(&self, symTable: &mut SymbolTable, inst: &Instruction, astFunc: &dyn Fn(&str) -> String) -> bool
    {
        if inst.0.len() != 3
        {
            println!("operand count not correct. Requires 3 got {} {:?}", inst.0.len(), &inst);
            return false;
        }
        let mut output = "".to_string();
        let (hasSat, dim) = self.get_opcode(symTable, inst.0.get(0).unwrap());
        let (mut output, mut mask) = self.get_dest(symTable, inst.0.get(1).unwrap());

        let mut exprPart = "".to_string();
        exprPart.push_str(&astFunc(&self.get_operand_output(symTable, mask.as_ref().unwrap(), inst.0.get(2).unwrap())));

        if hasSat
        {
            println!("{} saturate( {} )", output, exprPart);
        } else {
            println!("{} {}", output, exprPart);
        }
        return true;
    }

    fn parse_2_operands(&self, symTable: &mut SymbolTable, inst: &Instruction, astFunc: &dyn Fn(&str, &str) -> String) -> bool
    {
        if inst.0.len() != 4
        {
            println!("operand count not correct. Requires 4 got {} {:?}", inst.0.len(), &inst);
            return false;
        }

        let mut output = "".to_string();
        let (hasSat, dim) = self.get_opcode(symTable, inst.0.get(0).unwrap());
        let (mut output, mut mask) = self.get_dest(symTable, inst.0.get(1).unwrap());

        let mut exprPart = "".to_string();

        exprPart.push_str(&astFunc(
            &self.get_operand_output(symTable, mask.as_ref().unwrap(), inst.0.get(2).unwrap()),
            &self.get_operand_output(symTable, mask.as_ref().unwrap(), inst.0.get(3).unwrap())
        )
        );

        if hasSat
        {
            println!("{} saturate( {} )", output, exprPart);
        } else {
            println!("{} {}", output, exprPart);
        }
        return true;
    }

    fn parse_3_operands(&self, symTable: &mut SymbolTable, inst: &Instruction, astFunc: &dyn Fn(&str, &str, &str) -> String) -> bool
    {
        if inst.0.len() != 5
        {
            println!("operand count not correct. Requires 5 got {} {:?}", inst.0.len(), &inst);
            return false;
        }

        let mut output = "".to_string();
        let (hasSat, dim) = self.get_opcode(symTable, inst.0.get(0).unwrap());
        let (mut output, mut mask) = self.get_dest(symTable, inst.0.get(1).unwrap());

        let mut exprPart = "".to_string();

        exprPart.push_str(&astFunc(
            &self.get_operand_output(symTable, mask.as_ref().unwrap(), inst.0.get(2).unwrap()),
            &self.get_operand_output(symTable, mask.as_ref().unwrap(), inst.0.get(3).unwrap()),
            &self.get_operand_output(symTable, mask.as_ref().unwrap(), inst.0.get(4).unwrap())
        )
        );

        if hasSat
        {
            println!("{} saturate( {} )", output, exprPart);
        } else {
            println!("{} {}", output, exprPart);
        }
        return true;
    }

    fn get_opcode(&self, symTable: &mut SymbolTable, inst: &InstructionComponent) -> (bool, Option<u32>)
    {
        let mut hasSat = false;
        let mut dim = None;
        if let InstructionComponent::OpCode(opcode) = inst
        {
            if (opcode.Posts.len() > 0)
            {
                hasSat = true;
            }
            if (opcode.Dim.is_some())
            {
                dim = opcode.Dim.clone();
            }
        };
        (hasSat, dim)
    }

    fn get_dest(&self, symTable: &mut SymbolTable, inst: &InstructionComponent) -> (String, Option<SwizzlesMask>)
    {
        let mut mask: Option<SwizzlesMask>;
        let mut output = "".to_string();
        if let InstructionComponent::VarOperand(var) = inst
        {
            mask = Some(var.Swizzles.clone());
            let mut varName = var.var_name();
            let mut symName = symTable.get_symbol(&varName);
            // output attribute dont use local symbol name
            if varName.starts_with("o")
            {
                if symName.starts_with("local")
                {
                    varName = varName.replace("o", "_OUT.");
                    symName = &varName;
                } else {
                    varName = format!("_OUT.{}", &symName);
                    symName = &varName;
                }
            }
            output.push_str(&format!("{}{} = ", symName, &var.dim_swizzle_name()));
        } else {
            panic!();
        }

        let allMask = SwizzlesMask::all_mask();
        if mask.is_none()
        {
            mask = Some(allMask);
        }

        (output, mask)
    }


    fn get_operand_output(&self, symTable: &mut SymbolTable, mask: &SwizzlesMask, inst: &InstructionComponent) -> String
    {
        let mut output = "".to_string();
        if let InstructionComponent::VarOperand(var) = inst
        {
            let outputMask = var.get_swizzles(mask);
            let mut symName = symTable.get_symbol(&var.var_name());
            let mut varName = var.var_name();

            // constant buffer dont use symbol name
            if varName.starts_with("cb")
            {
                symName = "_CB";
            }
            // Input attribute dont use local symbol name
            if varName.starts_with("v")
            {
                if symName.starts_with("local")
                {
                    varName = varName.replace("v", "_IN.");
                    symName = &varName;
                } else {
                    varName = format!("_IN.{}", &symName);
                    symName = &varName;
                }
            }

            // output attribute dont use local symbol name
            if varName.starts_with("o")
            {
                if symName.starts_with("local")
                {
                    varName = varName.replace("o", "_OUT.");
                    symName = &varName;
                } else {
                    varName = format!("_OUT.{}", &symName);
                    symName = &varName;
                }
            }
            if var.IsTexture
            {
                varName = var.var_name();
                symName = &varName;
            }

            output.push_str(&format!("{}{}{}.{}", if var.Neg {"-"} else {""}, symName, &var.dim_name(), outputMask.to_string()));
        } else if let InstructionComponent::ConstantOperand(constant) = inst
        {
            output.push_str(&format!("{}", constant.output(mask)));
        }
        return output;
    }
}

fn main()
{
    if env::args().len() < 2
    {
        println!("dxbx_reader.exe file");
        return;
    }
    println!("{}",std::env::current_dir().unwrap().to_str().unwrap());
    println!("{}",std::env::current_exe().unwrap().to_str().unwrap());

    let args :std::vec::Vec<std::string::String>= env::args().into_iter().map(|c|{c}).collect();
    let mut symTable = SymbolTable::new();

    if args.len() > 2
    {
        let mut opts = "".to_string();
        for i in 1..args.len()-1
        {
            opts.push_str(" ");
            opts.push_str(&args[i]);
        }

        let definePattern = Regex::new(
            r#"(?x)
                [\t\ ]*
                (?:
                    IN
                        [\t\ ]*
                        =
                        [\t\ ]*
                        \{
                            (
                                (?:
                                    [\t\ ]*
                                    [A-Za-z_0-9]+
                                    [\t\ ]*
                                )*
                            )
                        \}
                )?
                (?:
                    OUT
                        [\t\ ]*
                        =
                        [\t\ ]*
                        \{
                            (
                                (?:
                                    [\t\ ]*
                                    [A-Za-z_0-9]+
                                    [\t\ ]*
                                )*
                            )
                        \}
                )?
            "#).unwrap();

        for it in definePattern.captures_iter(&opts)
        {
            if it.get(1).is_some()
            {
                let inDefine = it.get(1).as_ref().unwrap().as_str();
                let splits :Vec<&str>= inDefine.trim().split(" ").into_iter().collect();
                for i in 0..splits.len()
                {
                    symTable.Symbols.insert(format!("v{}", i), splits[i].trim().to_string());
                }
            }
            if it.get(2).is_some()
            {
                let inDefine = it.get(2).as_ref().unwrap().as_str();
                let splits :Vec<&str>= inDefine.trim().split(" ").into_iter().collect();
                for i in 0..splits.len()
                {
                    symTable.Symbols.insert(format!("o{}", i), splits[i].trim().to_string());
                }
            }
        }
        println!("{:?}",symTable);
    }
    let inputFile = &args[args.len()-1];

    //println!("Hello, world {:?}!",inputFile);
    let f = std::fs::OpenOptions::new().read(true).open(inputFile);
    if f.is_err()
    {
        println!("{} {:?}", inputFile, f.err().unwrap());
        return;
    }
    let f = f.unwrap();

    let mut parser = DXBCParser::new();
    for l in std::io::BufReader::new(f).lines()
    {
        let line = l.as_ref().unwrap().as_str().trim();
        if line.len() == 0
        {
            continue;
        }
        let mut newLine = line.to_string();
        newLine = newLine.trim().to_string();
        newLine.push('\n');
        let ret = DXBCLexer(&newLine);
        match ret
        {
            None=>panic!(),
            Some(inst)
            =>{
                parser.add(inst)
            }
        };
    }

    parser.parse(&mut symTable);
    return;
}
