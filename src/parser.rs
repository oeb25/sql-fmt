use pest;
use pest::Parser;

#[derive(Parser)]
#[grammar = "sql.pest"]
struct IdentParser;

#[derive(Debug)]
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
    self.next().ok_or(ParseError::Dunno)
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

trait Parseable: Sized {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Self, ParseError>;
}

#[derive(Debug)]
struct TableIdent {
  schema: String,
  name: String,
}

const DEFAULT_SCHEMA: &str = "default";

impl Parseable for TableIdent {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<TableIdent, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::var => TableIdent {
        schema: DEFAULT_SCHEMA.to_owned(),
        name: t.as_str().to_owned(),
      },
      Rule::schema_specifed => {
        let mut inp = t.into_inner();
        TableIdent {
          schema: inp.pop()?.as_str().to_owned(),
          name: inp.pop()?.as_str().to_owned(),
        }
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug)]
struct Ttype {
  schema: String,
  name: String,
}

impl Parseable for Ttype {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Ttype, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::var => Ttype {
        schema: DEFAULT_SCHEMA.to_owned(),
        name: t.as_str().to_owned(),
      },
      Rule::schema_specifed => {
        let mut inp = t.into_inner();
        Ttype {
          schema: inp.pop()?.as_str().to_owned(),
          name: inp.pop()?.as_str().to_owned(),
        }
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug)]
struct Expression {
  schema: String,
  name: String,
}

impl Parseable for Expression {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Expression, ParseError> {
    unimplemented!()
  }
}

#[derive(Debug)]
enum ColumnConstraintReferencesMatch {
  Full,
  Partial,
  Simple,
}

#[derive(Debug)]
struct ColumnConstraintReferences {
  table: TableIdent,
  column: Option<String>,
  mmatch: Option<ColumnConstraintReferencesMatch>,
}

impl Parseable for ColumnConstraintReferences {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<ColumnConstraintReferences, ParseError> {
    let t = inp.pop()?;
    let table = match t.as_rule() {
      Rule::table_ident => TableIdent::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    let t = inp.next();
    let (t, column) = match t {
      Some(t) => match t.as_rule() {
        Rule::column_references_column => {
          (inp.next(), Some(t.into_inner().pop()?.as_str().to_owned()))
        }
        _ => unparseable!(t),
      },
      None => (t, None),
    };
    let (t, mmatch) = match t {
      Some(t) => match t.as_rule() {
        Rule::column_references_match => unimplemented!(),
        _ => unparseable!(t),
      },
      None => (t, None),
    };

    Ok(ColumnConstraintReferences {
      table: table,
      column: column,
      mmatch: mmatch,
    })
  }
}

#[derive(Debug)]
enum ColumnConstraint {
  NotNull,
  Null,
  Check,
  Default,
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
      Rule::column_default => ColumnConstraint::Default,
      Rule::column_unique => ColumnConstraint::Unique,
      Rule::column_primary_key => ColumnConstraint::PrimaryKey,
      Rule::column_references => {
        ColumnConstraint::References(ColumnConstraintReferences::parse(t.into_inner())?)
      }
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug)]
struct CreateTableField {
  name: String,
  ttype: String,
  constraints: Option<Vec<ColumnConstraint>>,
}

impl Parseable for CreateTableField {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateTableField, ParseError> {
    let t = inp.pop()?;
    let name = match t.as_rule() {
      Rule::var => t.as_str().to_owned(),
      _ => unparseable!(t),
    };

    let t = inp.pop()?;
    let ttype = match t.as_rule() {
      Rule::var => t.as_str().to_owned(),
      _ => unparseable!(t),
    };

    // TODO COLLATE

    let constraints = match inp.pop().ok().map(|t| match t.as_rule() {
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

#[derive(Debug)]
struct CreateTable {
  name: TableIdent,
  if_not_exsists: bool,
  fields: Vec<CreateTableField>,
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

#[derive(Debug)]
enum SelectClauseItem {
  As(Box<SelectClauseItem>, String),
  Field(String),
  SubField(String, Box<SelectClauseItem>),
  All,
}

impl Parseable for SelectClauseItem {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<SelectClauseItem, ParseError> {
    let t = inp.pop()?;
    let f = match t.as_rule() {
      Rule::all => SelectClauseItem::All,
      Rule::select_clause_sub_field => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        let base = match t.as_rule() {
          Rule::var => t.as_str().to_owned(),
          _ => unparseable!(t),
        };

        let t = inp.pop()?;
        let sub = match t.as_rule() {
          Rule::var => SelectClauseItem::Field(t.as_str().to_owned()),
          Rule::all => SelectClauseItem::All,
          _ => unparseable!(t),
        };

        SelectClauseItem::SubField(base, Box::new(sub))
      }
      _ => unparseable!(t),
    };

    Ok(f)
  }
}

#[derive(Debug)]
struct SelectClause(Vec<SelectClauseItem>);

impl Parseable for SelectClause {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<SelectClause, ParseError> {
    Ok(SelectClause(
      a(inp.map(|t| SelectClauseItem::parse(t.into_inner())))?,
    ))
  }
}

#[derive(Debug)]
enum FromClause {
  Table(TableIdent),
  Schema(String, Box<FromClause>),
}

impl Parseable for FromClause {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<FromClause, ParseError> {
    unimplemented!()
  }
}

#[derive(Debug)]
struct Select {
  clause: SelectClause,
  from: FromClause,
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
      Rule::table_ident => FromClause::Table(TableIdent::parse(t.into_inner())?),
      _ => unparseable!(t),
    };

    Ok(Select {
      clause: select_clause,
      from: from_clause,
    })
  }
}

#[derive(Debug)]
struct CreateSchema(String);

impl Parseable for CreateSchema {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateSchema, ParseError> {
    let t = inp.pop()?;
    Ok(CreateSchema(t.as_str().to_owned()))
  }
}

#[derive(Debug)]
enum CreateFunctionArgMode {
  In,
  Out,
  Inout,
  Variadic,
}



impl Parseable for CreateFunctionArgMode {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateFunctionArgMode, ParseError> {
    unimplemented!()
  }
}

#[derive(Debug)]
struct CreateFunctionArg {
  mode: Option<CreateFunctionArgMode>,
  name: Option<TableIdent>,
  ttype: Ttype,
  default: Option<Expression>,
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

#[derive(Debug)]
enum CreateFunctionReturns {
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

#[derive(Debug)]
enum CreateFunctionBody {
  AsDef(String),
  Language(String),
}

impl Parseable for CreateFunctionBody {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateFunctionBody, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::create_function_body_as_def => CreateFunctionBody::AsDef(t.as_str().to_owned()),
      Rule::create_function_body_language => CreateFunctionBody::Language(t.as_str().to_owned()),
      _ => unparseable!(t),
    })
  }
}

#[derive(Debug)]
struct CreateFunction {
  name: TableIdent,
  or_replace: bool,
  args: Vec<CreateFunctionArg>,
  returns: CreateFunctionReturns,
  body: Vec<CreateFunctionBody>,
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
    let returns = match t.as_rule() {
      Rule::create_function_returns => CreateFunctionReturns::parse(t.into_inner()),
      _ => unparseable!(t),
    }?;
    let t = inp.pop()?;
    let body = match t.as_rule() {
      Rule::create_function_body_list => a(t.into_inner().map(|t| match t.as_rule() {
        Rule::create_function_body_item => CreateFunctionBody::parse(t.into_inner()),
        _ => unparseable!(t),
      })),
      _ => unparseable!(t),
    }?;

    Ok(CreateFunction {
      name: name,
      or_replace: or_replace,
      args: args,
      returns: returns,
      body: body,
    })
  }
}

#[derive(Debug)]
enum Statement {
  CreateTable(CreateTable),
  CreateSchema(CreateSchema),
  CreateFunction(CreateFunction),
  Select(Select),
}

impl Parseable for Statement {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Statement, ParseError> {
    let t = inp.pop()?;
    match t.as_rule() {
      Rule::create_table => CreateTable::parse(t.into_inner()).map(|c| Statement::CreateTable(c)),
      Rule::create_schema => {
        CreateSchema::parse(t.into_inner()).map(|c| Statement::CreateSchema(c))
      }
      Rule::create_function => {
        CreateFunction::parse(t.into_inner()).map(|c| Statement::CreateFunction(c))
      }
      Rule::select_stmt => Select::parse(t.into_inner()).map(|c| Statement::Select(c)),
      _ => unparseable!(t),
    }
  }
}

#[derive(Debug)]
pub struct Document(Vec<Statement>);

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
