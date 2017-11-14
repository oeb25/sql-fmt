use std;

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
  Raw(String),
  Escaped(String),
}

impl Var {
  pub fn contents<'a>(&'a self) -> &'a str {
    match self {
      &Var::Raw(ref s) => s,
      &Var::Escaped(ref s) => s,
    }
  }
  pub fn len(&self) -> usize {
    self.contents().len()
  }
}

impl std::cmp::PartialOrd for Var {
  fn partial_cmp(&self, b: &Var) -> Option<std::cmp::Ordering> {
    self.contents().partial_cmp(b.contents())
  }
}

#[derive(Debug, Clone)]
pub struct SelectClause(pub Vec<SelectClauseItem>);

#[derive(Debug, Clone)]
pub enum Expression {
  FunctionCall(FunctionCall),
  Ref(TableIdent),
}

#[derive(Debug, Clone)]
pub enum ColumnConstraintReferencesMatch {
  Full,
  Partial,
  Simple,
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

#[derive(Debug, Clone)]
pub enum ExprOrAll {
  All,
  Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum FromClause {
  Table(TableIdent),
}

#[derive(Debug, Clone)]
pub enum CreateFunctionArgMode {
  In,
  Out,
  Inout,
  Variadic,
}

#[derive(Debug, Clone)]
pub enum CreateFunctionReturns {
  Table(CreateTable),
  Ttype(Ttype),
  SetOf(Ttype),
}

#[derive(Debug, Clone)]
pub enum SqlString {
  Raw(String),
  DollarQuoted(String, String),
}

#[derive(Debug, Clone)]
pub enum CreateFunctionBody {
  AsDef(String),
  ParsedAsDef(Document),
  Language(Var),
}

#[derive(Debug, Clone)]
pub enum TransactionStmt {
  Begin,
  End,
}

#[derive(Debug, Clone)]
pub struct CreateSchema(pub Var);

#[derive(Debug, Clone)]
pub enum Statement {
  CreateTable(CreateTable),
  CreateSchema(CreateSchema),
  CreateFunction(CreateFunction),
  Select(Select),
  Transaction(TransactionStmt),
}

#[derive(Debug, Clone)]
pub struct TableIdent {
  pub schema: Option<Var>,
  pub name: Var,
}

#[derive(Debug, Clone)]
pub struct Ttype {
  pub schema: Option<Var>,
  pub name: Var,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
  pub base: TableIdent,
  pub args: Option<Vec<Expression>>,
}

#[derive(Debug, Clone)]
pub struct ColumnConstraintReferences {
  pub table: TableIdent,
  pub column: Option<Var>,
  pub mmatch: Option<ColumnConstraintReferencesMatch>,
}

#[derive(Debug, Clone)]
pub struct CreateTableField {
  pub name: Var,
  pub ttype: Ttype,
  pub constraints: Option<Vec<ColumnConstraint>>,
}

#[derive(Debug, Clone)]
pub struct CreateTable {
  pub name: TableIdent,
  pub if_not_exsists: bool,
  pub fields: Vec<CreateTableField>,
}

#[derive(Debug, Clone)]
pub struct SelectClauseItem {
  pub expr_or_all: ExprOrAll,
  pub ass: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Select {
  pub clause: SelectClause,
  pub from: FromClause,
}

#[derive(Debug, Clone)]
pub struct CreateFunctionArg {
  pub mode: Option<CreateFunctionArgMode>,
  pub name: Option<TableIdent>,
  pub ttype: Ttype,
  pub default: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct CreateFunction {
  pub name: TableIdent,
  pub or_replace: bool,
  pub args: Vec<CreateFunctionArg>,
  pub returns: CreateFunctionReturns,
  pub body: Vec<CreateFunctionBody>,
}

#[derive(Debug, Clone)]
pub struct Document(pub Vec<Statement>);
