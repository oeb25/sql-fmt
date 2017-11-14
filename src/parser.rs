use pest;
use pest::Parser;

#[derive(Parser)]
#[grammar = "sql.pest"]
pub struct IdentParser;

#[derive(Debug, Clone)]
pub enum ParseError {
  Dunno,
  Unparseable(String),
  More(Vec<ParseError>),
}

type P<'a> = pest::iterators::Pair<Rule, pest::inputs::StrInput<'a>>;

fn a<T, I: Iterator<Item = Result<T, ParseError>>>(a: I) -> Result<Vec<T>, ParseError> {
  let mut v = vec![];
  let mut errors = vec![];

  for n in a {
    match n {
      Ok(n) => v.push(n),
      Err(n) => errors.push(n),
    }
  }

  if errors.len() > 1 {
    Err(ParseError::More(errors))
  } else if errors.len() == 1 {
    Err(errors.remove(0))
  } else {
    Ok(v)
  }
}

trait Ps<'a>: Iterator<Item = P<'a>> {
  fn pop(&mut self) -> Result<P<'a>, ParseError> {
    // self.next().ok_or(ParseError::Dunno)
    Ok(self.next().ok_or(ParseError::Dunno).unwrap())
  }
}

impl<'a, T> Ps<'a> for T
where
  T: Iterator<Item = P<'a>>,
{
}

macro_rules! unparseable {
    ($fmt:expr) => ({
        // panic!(concat!("internal error: entered unparseable code: ", $fmt), $($arg)*)
        return Err(ParseError::Unparseable(format!("{:?} : {}:{}", $fmt, file!(), line!())))
    });
}

macro_rules! rule {
  ($t:expr, { $( $x:ident => $b:expr )* }) => {
    match $t.as_rule() {
      $(
        Rule::$x => $b,
      )*
      _ => unparseable!($t)
    }
  };
}

trait Parseable: Sized {
  fn parse<'a, I: Ps<'a>>(inp: I) -> Result<Self, ParseError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Var {
  Raw(String),
  Escaped(String),
}
use std;

impl Var {
  fn contents<'a>(&'a self) -> &'a str {
    match self {
      &Var::Raw(ref s) => s,
      &Var::Escaped(ref s) => s,
    }
  }
}

impl std::cmp::PartialOrd for Var {
  fn partial_cmp(&self, b: &Var) -> Option<std::cmp::Ordering> {
    self.contents().partial_cmp(b.contents())
  }
}

impl Parseable for Var {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Var, ParseError> {
    let t = inp.pop()?;

    Ok(rule!(t, {
      escaped_var => {
        let s = t.as_str().to_owned();
        Var::Escaped((&s[1..s.len() - 1]).to_owned())
      }
      var => Var::Raw(t.as_str().to_owned())
    }))
  }
}

#[derive(Debug, Clone)]
pub struct TableIdent {
  pub schema: Option<Var>,
  pub name: Var,
}

impl Parseable for TableIdent {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<TableIdent, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::s_var => TableIdent {
        schema: None,
        name: Var::parse(t.into_inner())?,
      },
      Rule::schema_specifed => {
        let mut inp = t.into_inner();
        TableIdent {
          schema: Some(Var::parse(inp.pop()?.into_inner())?),
          name: Var::parse(inp.pop()?.into_inner())?,
        }
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug, Clone)]
pub struct Ttype {
  pub schema: Option<Var>,
  pub name: Var,
}

impl Parseable for Ttype {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Ttype, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::s_var => Ttype {
        schema: None,
        name: Var::parse(t.into_inner())?,
      },
      Rule::schema_specifed => {
        let mut inp = t.into_inner();
        Ttype {
          schema: Some(Var::parse(inp.pop()?.into_inner())?),
          name: Var::parse(inp.pop()?.into_inner())?,
        }
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
  pub base: TableIdent,
  pub args: Option<Vec<Expression>>,
}

impl Parseable for FunctionCall {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<FunctionCall, ParseError> {
    let t = inp.pop()?;
    let name = match t.as_rule() {
      Rule::function_name => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        rule!(t, {
          table_ident => TableIdent::parse(t.into_inner())
        })
      }
      _ => unparseable!(t),
    }?;

    let t = inp.next();
    let args = match t {
      Some(t) => Some(a(match t.as_rule() {
        Rule::function_arguments => t.into_inner().map(|t| match t.as_rule() {
          Rule::expr => Expression::parse(t.into_inner()),
          _ => unparseable!(t),
        }),
        _ => unparseable!(t),
      })?),
      None => None,
    };

    Ok(FunctionCall {
      base: name,
      args: args,
    })
  }
}

#[derive(Debug, Clone)]
pub enum Expression {
  FunctionCall(FunctionCall),
}

impl Parseable for Expression {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Expression, ParseError> {
    let t = inp.pop()?;
    rule!(t, {
      function_call => {
        FunctionCall::parse(t.into_inner()).map(|c| Expression::FunctionCall(c))
      }
    })
  }
}

#[derive(Debug, Clone)]
pub enum ColumnConstraintReferencesMatch {
  Full,
  Partial,
  Simple,
}

#[derive(Debug, Clone)]
pub struct ColumnConstraintReferences {
  pub table: TableIdent,
  pub column: Option<Var>,
  pub mmatch: Option<ColumnConstraintReferencesMatch>,
}

impl Parseable for ColumnConstraintReferences {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<ColumnConstraintReferences, ParseError> {
    let t = inp.pop()?;
    let table = rule!(t, {
      table_ident => TableIdent::parse(t.into_inner())?
    });

    let t = inp.next();
    let (t, column) = match t {
      Some(t) => rule!(t, {
        column_references_column => (
          inp.next(),
          Some(Var::parse(t.into_inner().pop()?.into_inner())?),
        )
      }),
      None => (t, None),
    };
    let (t, mmatch) = match t {
      Some(t) => rule!(t, {
        column_references_match => unimplemented!()
      }),
      None => (t, None),
    };

    Ok(ColumnConstraintReferences {
      table: table,
      column: column,
      mmatch: mmatch,
    })
  }
}

#[derive(Debug, Clone)]
pub enum ColumnConstraint {
  NotNull,
  Null,
  Check,
  Default(Expression),
  Unique,
  PrimaryKey,
  References(ColumnConstraintReferences),
}

impl Parseable for ColumnConstraint {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<ColumnConstraint, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::column_not_null => ColumnConstraint::NotNull,
      Rule::column_null => ColumnConstraint::Null,
      Rule::column_check => ColumnConstraint::Check,
      Rule::column_default => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        match t.as_rule() {
          Rule::expr => ColumnConstraint::Default(Expression::parse(t.into_inner())?),
          _ => unparseable!(t),
        }
      }
      Rule::column_unique => ColumnConstraint::Unique,
      Rule::column_primary_key => ColumnConstraint::PrimaryKey,
      Rule::column_references => {
        ColumnConstraint::References(ColumnConstraintReferences::parse(t.into_inner())?)
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug, Clone)]
pub struct CreateTableField {
  pub name: Var,
  pub ttype: Ttype,
  pub constraints: Option<Vec<ColumnConstraint>>,
}

impl Parseable for CreateTableField {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateTableField, ParseError> {
    let t = inp.pop()?;
    let name = match t.as_rule() {
      Rule::s_var => Var::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    let t = inp.pop()?;
    let ttype = match t.as_rule() {
      Rule::ttype => Ttype::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    // TODO COLLATE

    let constraints = match inp.next().map(|t| match t.as_rule() {
      Rule::column_constraints => a(t.into_inner().map(|t| match t.as_rule() {
        Rule::column_constraint => ColumnConstraint::parse(t.into_inner()),
        _ => unparseable!(t),
      })),
      _ => unparseable!(t),
    }) {
      Some(r) => Some(r?),
      None => None,
    };

    Ok(CreateTableField {
      name: name,
      ttype: ttype,
      constraints: constraints,
    })
  }
}

#[derive(Debug, Clone)]
pub struct CreateTable {
  pub name: TableIdent,
  pub if_not_exsists: bool,
  pub fields: Vec<CreateTableField>,
}

impl Parseable for CreateTable {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateTable, ParseError> {
    let t = inp.pop()?;
    let (t, if_not_exsists) = match t.as_rule() {
      Rule::table_ident => (t, false),
      Rule::if_not_exists => (inp.pop()?, true),
      _ => unparseable!(t),
    };

    let name = match t.as_rule() {
      Rule::table_ident => TableIdent::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    let fields = a(inp.map(|t| CreateTableField::parse(t.into_inner())))?;

    Ok(CreateTable {
      name: name,
      if_not_exsists: if_not_exsists,
      fields: fields,
    })
  }
}

#[derive(Debug, Clone)]
pub enum ExprOrAll {
  All,
  Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct SelectClauseItem {
  pub expr_or_all: ExprOrAll,
  pub ass: Option<String>,
}

impl Parseable for SelectClauseItem {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<SelectClauseItem, ParseError> {
    let t = inp.pop()?;
    let expr_or_all = match t.as_rule() {
      Rule::all => ExprOrAll::All,
      Rule::expr => ExprOrAll::Expression(Expression::parse(t.into_inner())?),
      _ => unparseable!(t),
    };

    let t = inp.next();
    let ass = match t {
      Some(t) => match t.as_rule() {
        Rule::select_from => None,
        _ => unparseable!(t),
      },
      None => None,
    };

    Ok(SelectClauseItem {
      expr_or_all: expr_or_all,
      ass: ass,
    })
  }
}

#[derive(Debug, Clone)]
pub struct SelectClause(pub Vec<SelectClauseItem>);

impl Parseable for SelectClause {
  fn parse<'a, I: Ps<'a>>(inp: I) -> Result<SelectClause, ParseError> {
    Ok(SelectClause(
      a(inp.map(|t| SelectClauseItem::parse(t.into_inner())))?,
    ))
  }
}

#[derive(Debug, Clone)]
pub enum FromClause {
  Table(TableIdent),
}

impl Parseable for FromClause {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<FromClause, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::select_from_item => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;

        match t.as_rule() {
          Rule::table_ident => FromClause::Table(TableIdent::parse(t.into_inner())?),
          _ => unparseable!(t),
        }
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug, Clone)]
pub struct Select {
  pub clause: SelectClause,
  pub from: FromClause,
}

impl Parseable for Select {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Select, ParseError> {
    let t = inp.pop()?;
    let select_clause = match t.as_rule() {
      Rule::select_clause => SelectClause::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    let t = inp.pop()?;
    let from_clause = match t.as_rule() {
      Rule::select_from => FromClause::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    Ok(Select {
      clause: select_clause,
      from: from_clause,
    })
  }
}

#[derive(Debug, Clone)]
pub struct CreateSchema(pub Var);

impl Parseable for CreateSchema {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateSchema, ParseError> {
    let t = inp.pop()?;
    Ok(CreateSchema(Var::parse(t.into_inner())?))
  }
}

#[derive(Debug, Clone)]
pub enum CreateFunctionArgMode {
  In,
  Out,
  Inout,
  Variadic,
}



impl Parseable for CreateFunctionArgMode {
  fn parse<'a, I: Ps<'a>>(_inp: I) -> Result<CreateFunctionArgMode, ParseError> {
    unimplemented!()
  }
}

#[derive(Debug, Clone)]
pub struct CreateFunctionArg {
  pub mode: Option<CreateFunctionArgMode>,
  pub name: Option<TableIdent>,
  pub ttype: Ttype,
  pub default: Option<Expression>,
}

impl Parseable for CreateFunctionArg {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateFunctionArg, ParseError> {
    let t = inp.pop()?;
    let (t, mode) = match t.as_rule() {
      Rule::create_function_arg_mode => (
        inp.pop()?,
        Some(CreateFunctionArgMode::parse(t.into_inner())?),
      ),
      Rule::table_ident => (t, None),
      _ => unparseable!(t),
    };
    let (t, name) = match t.as_rule() {
      Rule::table_ident => (inp.pop()?, Some(TableIdent::parse(t.into_inner())?)),
      _ => unparseable!(t),
    };
    let ttype = match t.as_rule() {
      Rule::ttype => Ttype::parse(t.into_inner())?,
      _ => unparseable!(t),
    };
    let t = inp.next();
    let default = match t {
      Some(t) => match t.as_rule() {
        Rule::create_function_arg_default => {
          let mut inp = t.into_inner();
          let t = inp.pop()?;
          match t.as_rule() {
            Rule::expr => Some(Expression::parse(t.into_inner())?),
            _ => unparseable!(t),
          }
        }
        _ => unparseable!(t),
      },
      None => None,
    };

    Ok(CreateFunctionArg {
      mode: mode,
      name: name,
      ttype: ttype,
      default: default,
    })
  }
}

#[derive(Debug, Clone)]
pub enum CreateFunctionReturns {
  Table(CreateTable),
  Ttype(Ttype),
  SetOf(Ttype),
}

impl Parseable for CreateFunctionReturns {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateFunctionReturns, ParseError> {
    let t = inp.pop()?;
    match t.as_rule() {
      Rule::create_table => {
        CreateTable::parse(t.into_inner()).map(|c| CreateFunctionReturns::Table(c))
      }
      Rule::setof => {
        Ttype::parse(t.into_inner().pop()?.into_inner()).map(|c| CreateFunctionReturns::SetOf(c))
      }
      Rule::ttype => Ttype::parse(t.into_inner()).map(|c| CreateFunctionReturns::Ttype(c)),
      _ => unparseable!(t),
    }
  }
}

#[derive(Debug, Clone)]
pub enum SqlString {
  Raw(String),
  DollarQuoted(String, String),
}

impl SqlString {
  fn contents(self) -> String {
    match self {
      SqlString::Raw(s) => s,
      SqlString::DollarQuoted(_, s) => s,
    }
  }
}

impl Parseable for SqlString {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<SqlString, ParseError> {
    let t = inp.pop()?;
    match t.as_rule() {
      Rule::dollar_quoted => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        let p = match t.as_rule() {
          Rule::dollar_quoted_start => t.as_str().to_owned(),
          _ => unparseable!(t),
        };
        let t = inp.pop()?;
        let contents = match t.as_rule() {
          Rule::dollar_inners => t.as_str().to_owned(),
          _ => unparseable!(t),
        };
        Ok(SqlString::DollarQuoted(p, contents))
      }
      _ => unparseable!(t),
    }
  }
}

#[derive(Debug, Clone)]
pub enum CreateFunctionBody {
  AsDef(String),
  ParsedAsDef(Document),
  Language(Var),
}

impl Parseable for CreateFunctionBody {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateFunctionBody, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::create_function_body_as_def => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        match t.as_rule() {
          Rule::string => {
            let s = SqlString::parse(t.into_inner())?.contents();

            match Document::parse(&s) {
              Ok(inner) => CreateFunctionBody::ParsedAsDef(inner),
              Err(e) => return Err(e),
              // Err(_) => CreateFunctionBody::AsDef(s),
            }
          }
          _ => unparseable!(t),
        }
      }
      Rule::create_function_body_language => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        match t.as_rule() {
          Rule::s_var => CreateFunctionBody::Language(Var::parse(t.into_inner())?),
          _ => unparseable!(t),
        }
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug, Clone)]
pub struct CreateFunction {
  pub name: TableIdent,
  pub or_replace: bool,
  pub args: Vec<CreateFunctionArg>,
  pub returns: CreateFunctionReturns,
  pub body: Vec<CreateFunctionBody>,
}

impl Parseable for CreateFunction {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateFunction, ParseError> {
    let t = inp.pop()?;
    let (t, or_replace) = match t.as_rule() {
      Rule::or_replace => (inp.pop()?, true),
      Rule::table_ident => (t, false),
      _ => unparseable!(t),
    };
    let name = match t.as_rule() {
      Rule::table_ident => TableIdent::parse(t.into_inner())?,
      _ => unparseable!(t),
    };
    let t = inp.pop()?;
    let args = match t.as_rule() {
      Rule::create_function_args => a(t.into_inner().map(|t| match t.as_rule() {
        Rule::create_function_arg => CreateFunctionArg::parse(t.into_inner()),
        _ => unparseable!(t),
      })),
      _ => unparseable!(t),
    }?;
    let t = inp.pop()?;
    let returns = rule!(t, {
      create_function_returns => CreateFunctionReturns::parse(t.into_inner())
    })?;
    let t = inp.pop()?;
    let body = rule!(t, {
      create_function_body_list => a(t.into_inner().map(|t| rule!(t, {
        create_function_body_item => CreateFunctionBody::parse(t.into_inner())
      })))
    })?;

    Ok(CreateFunction {
      name: name,
      or_replace: or_replace,
      args: args,
      returns: returns,
      body: body,
    })
  }
}

#[derive(Debug, Clone)]
pub enum TransactionStmt {
  Begin,
  End,
}

impl Parseable for TransactionStmt {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<TransactionStmt, ParseError> {
    let t = inp.pop()?;
    Ok(rule!(t, {
      begin_stmt => TransactionStmt::Begin
      end_stmt => TransactionStmt::End
    }))
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  CreateTable(CreateTable),
  CreateSchema(CreateSchema),
  CreateFunction(CreateFunction),
  Select(Select),
  Transaction(TransactionStmt),
}

impl Parseable for Statement {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Statement, ParseError> {
    let t = inp.pop()?;
    rule!(t, {
      create_table => CreateTable::parse(t.into_inner()).map(|c| Statement::CreateTable(c))
      create_schema => CreateSchema::parse(t.into_inner()).map(|c| Statement::CreateSchema(c))
      create_function => CreateFunction::parse(t.into_inner()).map(|c| Statement::CreateFunction(c))
      transaction_stmt => TransactionStmt::parse(t.into_inner()).map(|c| Statement::Transaction(c))
      select_stmt => Select::parse(t.into_inner()).map(|c| Statement::Select(c))
    })
  }
}

#[derive(Debug, Clone)]
pub struct Document(pub Vec<Statement>);

impl Document {
  pub fn parse(src: &str) -> Result<Document, ParseError> {
    let t = IdentParser::parse_str(Rule::document, src)
      .unwrap_or_else(|e| panic!("{}", e))
      .next()
      .unwrap();
    match t.as_rule() {
      Rule::document => Ok(Document(
        a(t.into_inner().filter_map(|t| match t.as_rule() {
          Rule::stmt => Some(Statement::parse(t.into_inner())),
          Rule::comment => None,
          _ => unreachable!("{:?}", t),
        }))?,
      )),
      _ => unparseable!(t),
    }
  }
}
