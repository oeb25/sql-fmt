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
pub struct Cast(pub Expression, pub Ttype);

#[derive(Debug, Clone)]
pub enum Number {
  Int(i64),
  Float(f64),
}

#[derive(Debug, Clone)]
pub enum Expression {
  Infix(Operator, Box<(Expression, Expression)>),
  FunctionCall(FunctionCall),
  Ref(TableIdent),
  Select(Box<Select>),
  String(SqlString),
  Insert(InsertStmt),
  Cast(Box<Cast>),
  Number(Number),
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
pub struct FromClause {
  pub from: Expression,
  pub ass: Option<Var>,
}

#[derive(Debug, Clone)]
pub enum CreateFunctionArgMode {
  // In,
  // Out,
  // Inout,
  // Variadic,
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
pub struct View {
  pub name: TableIdent,
  pub ass: Select,
}

#[derive(Debug, Clone)]
pub enum Statement {
  CreateTable(CreateTable),
  CreateSchema(CreateSchema),
  CreateFunction(CreateFunction),
  Select(Select),
  Transaction(TransactionStmt),
  Return(ReturnStmt),
  Insert(InsertStmt),
  View(View),
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

impl Ttype {
  pub fn len(&self) -> usize {
    (match self.schema {
      Some(ref v) => v.len(),
      None => 0,
    }) + self.name.len()
  }
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
pub struct CreateTableColumn {
  pub name: Var,
  pub ttype: Ttype,
  pub constraints: Option<Vec<ColumnConstraint>>,
}

impl CreateTableColumn {
  pub fn is_primary_key(&self) -> bool {
    match self.constraints {
      Some(ref constraints) => for c in constraints {
        match c {
          ColumnConstraint::PrimaryKey => return true,
          _ => (),
        }
      },
      None => (),
    }

    false
  }
  pub fn is_not_null(&self) -> bool {
    match self.constraints {
      Some(ref constraints) => for c in constraints {
        match c {
          ColumnConstraint::NotNull => return true,
          ColumnConstraint::Null => return false,
          _ => (),
        }
      },
      None => (),
    }

    false
  }
}

#[derive(Debug, Clone)]
pub enum Operator {
  Equal,
  And,
  Add,
  As,
  AndLit,
  OrLit,
  Greater,
  Less,
  GreaterEqual,
  LessEqual,
  Contains,
  ContainedBy,
}

#[derive(Debug, Clone)]
pub struct CreateTableExcludeWith(pub Expression, pub Operator);

#[derive(Debug, Clone)]
pub struct CreateTableExclude {
  pub using: Option<TableIdent>,
  pub with: Vec<CreateTableExcludeWith>,
}

#[derive(Debug, Clone)]
pub enum CreateTableField {
  Column(CreateTableColumn),
  Unique(Var),
  Exclude(CreateTableExclude),
}

#[derive(Debug, Clone)]
pub struct CreateTable {
  pub name: TableIdent,
  pub if_not_exsists: bool,
  pub fields: Vec<CreateTableField>,
  pub inherits: Option<TableIdent>,
}

#[derive(Debug, Clone)]
pub struct SelectClauseItem {
  pub expr_or_all: ExprOrAll,
  pub ass: Option<Var>,
}

#[derive(Debug, Clone)]
pub enum Limit {
  All,
  Count(Expression),
}

#[derive(Debug, Clone)]
pub struct Select {
  pub with: Option<With>,
  pub clause: SelectClause,
  pub from: Option<Vec<FromClause>>,
  pub condition: Option<Expression>,
  pub limit: Option<Limit>,
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

#[derive(Debug, Clone)]
pub enum ReturnStmt {
  Query(Expression),
  Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct WithItem {
  pub name: Var,
  pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct With(pub Vec<WithItem>);

#[derive(Debug, Clone)]
pub enum ExprOrDefault {
  Expression(Expression),
  Default,
}

#[derive(Debug, Clone)]
pub enum InsertValues {
  Default,
  Values(Vec<ExprOrDefault>),
  Select(Box<Select>),
}

#[derive(Debug, Clone)]
pub enum InsertConflictUpdate {}

#[derive(Debug, Clone)]
pub struct InsertConflict {
  pub target: Vec<Var>,
  pub action: InsertConflictAction,
}

#[derive(Debug, Clone)]
pub enum InsertConflictAction {
  Nothing,
  Update(InsertConflictUpdate),
}

#[derive(Debug, Clone)]
pub enum InsertReturn {
  All,
}

#[derive(Debug, Clone)]
pub struct InsertStmt {
  pub with: Option<With>,
  pub table: TableIdent,
  pub columns: Option<Vec<Var>>,
  pub values: InsertValues,
  pub conflict: Option<InsertConflict>,
  pub ret: Option<InsertReturn>,
}
