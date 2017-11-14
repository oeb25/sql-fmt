use std::rc::Rc;

pub struct Visitor {
  stack: Vec<AstNode>,
}

impl Iterator for Visitor {
  type Item = AstNode;

  fn next(&mut self) -> Option<Self::Item> {
    let head = self.stack.pop();
    match head {
      Some(head) => {
        let mut children = head.children();
        children.append(&mut self.stack);
        self.stack = children;
        Some(head)
      }
      None => None,
    }
  }
}

pub trait Visit: Sized {
  fn ast(&self) -> AstNode;
  fn children(&self) -> Vec<AstNode>;
  fn visit<'a>(self) -> Visitor {
    Visitor {
      stack: vec![self.ast().clone()],
    }
  }
}

use parser::*;

#[derive(Debug, Clone)]
pub enum AstNode {
  Var(Rc<Var>),
  TableIdent(Rc<TableIdent>),
  Ttype(Rc<Ttype>),
  FunctionCall(Rc<FunctionCall>),
  ColumnConstraintReferences(Rc<ColumnConstraintReferences>),
  CreateTableField(Rc<CreateTableField>),
  CreateTable(Rc<CreateTable>),
  SelectClause(Rc<SelectClause>),
  Select(Rc<Select>),
  CreateSchema(Rc<CreateSchema>),
  CreateFunctionArg(Rc<CreateFunctionArg>),
  CreateFunction(Rc<CreateFunction>),
  Expression(Rc<Expression>),
  ColumnConstraintReferencesMatch(Rc<ColumnConstraintReferencesMatch>),
  ColumnConstraint(Rc<ColumnConstraint>),
  ExprOrAll(Rc<ExprOrAll>),
  SelectClauseItem(Rc<SelectClauseItem>),
  FromClause(Rc<FromClause>),
  CreateFunctionArgMode(Rc<CreateFunctionArgMode>),
  CreateFunctionReturns(Rc<CreateFunctionReturns>),
  SqlString(Rc<SqlString>),
  CreateFunctionBody(Rc<CreateFunctionBody>),
  TransactionStmt(Rc<TransactionStmt>),
  Statement(Rc<Statement>),
  Document(Rc<Document>),

  More(Vec<AstNode>),
}


impl Visit for AstNode {
  fn children(&self) -> Vec<AstNode> {
    match *self {
      AstNode::Var(ref s) => s.children(),
      AstNode::TableIdent(ref s) => s.children(),
      AstNode::Ttype(ref s) => s.children(),
      AstNode::FunctionCall(ref s) => s.children(),
      AstNode::ColumnConstraintReferences(ref s) => s.children(),
      AstNode::CreateTableField(ref s) => s.children(),
      AstNode::CreateTable(ref s) => s.children(),
      AstNode::SelectClause(ref s) => s.children(),
      AstNode::Select(ref s) => s.children(),
      AstNode::CreateSchema(ref s) => s.children(),
      AstNode::CreateFunctionArg(ref s) => s.children(),
      AstNode::CreateFunction(ref s) => s.children(),
      AstNode::Expression(ref s) => s.children(),
      AstNode::ColumnConstraintReferencesMatch(ref s) => s.children(),
      AstNode::ColumnConstraint(ref s) => s.children(),
      AstNode::ExprOrAll(ref s) => s.children(),
      AstNode::SelectClauseItem(ref s) => s.children(),
      AstNode::FromClause(ref s) => s.children(),
      AstNode::CreateFunctionArgMode(ref s) => s.children(),
      AstNode::CreateFunctionReturns(ref s) => s.children(),
      AstNode::SqlString(ref s) => s.children(),
      AstNode::CreateFunctionBody(ref s) => s.children(),
      AstNode::TransactionStmt(ref s) => s.children(),
      AstNode::Statement(ref s) => s.children(),
      AstNode::Document(ref s) => s.children(),

      AstNode::More(ref s) => s.clone(),
    }
  }
  fn ast(&self) -> AstNode {
    match *self {
      AstNode::Var(ref s) => s.ast(),
      AstNode::TableIdent(ref s) => s.ast(),
      AstNode::Ttype(ref s) => s.ast(),
      AstNode::FunctionCall(ref s) => s.ast(),
      AstNode::ColumnConstraintReferences(ref s) => s.ast(),
      AstNode::CreateTableField(ref s) => s.ast(),
      AstNode::CreateTable(ref s) => s.ast(),
      AstNode::SelectClause(ref s) => s.ast(),
      AstNode::Select(ref s) => s.ast(),
      AstNode::CreateSchema(ref s) => s.ast(),
      AstNode::CreateFunctionArg(ref s) => s.ast(),
      AstNode::CreateFunction(ref s) => s.ast(),
      AstNode::Expression(ref s) => s.ast(),
      AstNode::ColumnConstraintReferencesMatch(ref s) => s.ast(),
      AstNode::ColumnConstraint(ref s) => s.ast(),
      AstNode::ExprOrAll(ref s) => s.ast(),
      AstNode::SelectClauseItem(ref s) => s.ast(),
      AstNode::FromClause(ref s) => s.ast(),
      AstNode::CreateFunctionArgMode(ref s) => s.ast(),
      AstNode::CreateFunctionReturns(ref s) => s.ast(),
      AstNode::SqlString(ref s) => s.ast(),
      AstNode::CreateFunctionBody(ref s) => s.ast(),
      AstNode::TransactionStmt(ref s) => s.ast(),
      AstNode::Statement(ref s) => s.ast(),
      AstNode::Document(ref s) => s.ast(),

      AstNode::More(ref s) => AstNode::More(s.clone()),
    }
  }
}

impl Visit for Var {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::Var(Rc::new(self.clone()))
  }
}
impl Visit for TableIdent {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::TableIdent(Rc::new(self.clone()))
  }
}
impl Visit for Ttype {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::Ttype(Rc::new(self.clone()))
  }
}
impl Visit for FunctionCall {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::FunctionCall(Rc::new(self.clone()))
  }
}
impl Visit for ColumnConstraintReferences {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::ColumnConstraintReferences(Rc::new(self.clone()))
  }
}
impl Visit for CreateTableField {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateTableField(Rc::new(self.clone()))
  }
}
impl Visit for CreateTable {
  fn children(&self) -> Vec<AstNode> {
    vec![
      self.name.ast(),
      // pub if_not_exsists: bool,
      a(&self.fields),
    ]
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateTable(Rc::new(self.clone()))
  }
}
impl Visit for SelectClause {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::SelectClause(Rc::new(self.clone()))
  }
}
impl Visit for Select {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::Select(Rc::new(self.clone()))
  }
}
impl Visit for CreateSchema {
  fn children(&self) -> Vec<AstNode> {
    vec![]
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateSchema(Rc::new(self.clone()))
  }
}
impl Visit for CreateFunctionArg {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateFunctionArg(Rc::new(self.clone()))
  }
}
fn a<T: Visit>(s: &Vec<T>) -> AstNode {
  AstNode::More(s.iter().map(|x| x.ast()).collect())
}
impl Visit for CreateFunction {
  fn children(&self) -> Vec<AstNode> {
    vec![
      self.name.ast(),
      // self.or_replace.ast(), TODO: Bools??
      a(&self.args),
      self.returns.ast(),
      a(&self.body),
    ]
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateFunction(Rc::new(self.clone()))
  }
}
impl Visit for Expression {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::Expression(Rc::new(self.clone()))
  }
}
impl Visit for ColumnConstraintReferencesMatch {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::ColumnConstraintReferencesMatch(Rc::new(self.clone()))
  }
}
impl Visit for ColumnConstraint {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::ColumnConstraint(Rc::new(self.clone()))
  }
}
impl Visit for ExprOrAll {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::ExprOrAll(Rc::new(self.clone()))
  }
}
impl Visit for SelectClauseItem {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::SelectClauseItem(Rc::new(self.clone()))
  }
}
impl Visit for FromClause {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::FromClause(Rc::new(self.clone()))
  }
}
impl Visit for CreateFunctionArgMode {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateFunctionArgMode(Rc::new(self.clone()))
  }
}
impl Visit for CreateFunctionReturns {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateFunctionReturns(Rc::new(self.clone()))
  }
}
impl Visit for SqlString {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::SqlString(Rc::new(self.clone()))
  }
}
impl Visit for CreateFunctionBody {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::CreateFunctionBody(Rc::new(self.clone()))
  }
}
impl Visit for TransactionStmt {
  fn children(&self) -> Vec<AstNode> {
    unimplemented!()
  }
  fn ast(&self) -> AstNode {
    AstNode::TransactionStmt(Rc::new(self.clone()))
  }
}
impl Visit for Statement {
  fn children(&self) -> Vec<AstNode> {
    match *self {
      Statement::CreateTable(ref s) => vec![s.ast()],
      Statement::CreateSchema(ref s) => vec![s.ast()],
      Statement::CreateFunction(ref s) => vec![s.ast()],
      Statement::Select(ref s) => vec![s.ast()],
      Statement::Transaction(ref s) => vec![s.ast()],
    }
  }
  fn ast(&self) -> AstNode {
    AstNode::Statement(Rc::new(self.clone()))
  }
}
impl Visit for Document {
  fn children(&self) -> Vec<AstNode> {
    self.0.iter().map(|c| c.ast()).collect()
  }
  fn ast(&self) -> AstNode {
    AstNode::Document(Rc::new(self.clone()))
  }
}
