use std::rc::Rc;

pub enum Transform<T> {
  None,
  Skip,
  Replace(T),
}

pub trait Transformable: Sized {
  fn transform(&mut self) -> Transform<Self>;
  fn perform_transform(&mut self) {
    unimplemented!()
  }
}

use parser;

impl Transformable for parser::Document {
  fn perform_transform(&mut self) {
    match self.transform() {
      Transform::None => for c in self.0.iter_mut() {
        c.perform_transform();
      },
      Transform::Skip => {}
      Transform::Replace(t) => {
        t.perform_transform();
      }
    }
  }
  fn transform(&mut self) -> Transform<parser::Document> {
    Transform::None
  }
}

impl Transformable for parser::Statement {
  fn perform_transform(&mut self) {
    match self.transform() {
      Transform::None => match self {
        &mut parser::Statement::CreateTable(ref mut c) => c.perform_transform(),
        &mut parser::Statement::CreateSchema(ref mut c) => c.perform_transform(),
        &mut parser::Statement::CreateFunction(ref mut c) => c.perform_transform(),
        &mut parser::Statement::Select(ref mut c) => c.perform_transform(),
        &mut parser::Statement::Transaction(ref mut c) => c.perform_transform(),
      },
      Transform::Skip => {}
      Transform::Replace(t) => t,
    }
  }
  fn transform(&mut self) -> Transform<parser::Statement> {
    Transform::None
  }
}

// impl Visit for Var {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::Var(Rc::new(self.clone()))
//   }
// }
// impl Visit for TableIdent {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::TableIdent(Rc::new(self.clone()))
//   }
// }
// impl Visit for Ttype {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::Ttype(Rc::new(self.clone()))
//   }
// }
// impl Visit for FunctionCall {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::FunctionCall(Rc::new(self.clone()))
//   }
// }
// impl Visit for ColumnConstraintReferences {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::ColumnConstraintReferences(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateTableField {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateTableField(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateTable {
//   fn children(&self) -> Vec<AstNode> {
//     vec![
//       self.name.ast(),
//       // pub if_not_exsists: bool,
//       a(&self.fields),
//     ]
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateTable(Rc::new(self.clone()))
//   }
// }
// impl Visit for SelectClause {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::SelectClause(Rc::new(self.clone()))
//   }
// }
// impl Visit for Select {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::Select(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateSchema {
//   fn children(&self) -> Vec<AstNode> {
//     vec![]
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateSchema(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateFunctionArg {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateFunctionArg(Rc::new(self.clone()))
//   }
// }
// fn a<T: Visit>(s: &Vec<T>) -> AstNode {
//   AstNode::More(s.iter().map(|x| x.ast()).collect())
// }
// impl Visit for CreateFunction {
//   fn children(&self) -> Vec<AstNode> {
//     vec![
//       self.name.ast(),
//       // self.or_replace.ast(), TODO: Bools??
//       a(&self.args),
//       self.returns.ast(),
//       a(&self.body),
//     ]
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateFunction(Rc::new(self.clone()))
//   }
// }
// impl Visit for Expression {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::Expression(Rc::new(self.clone()))
//   }
// }
// impl Visit for ColumnConstraintReferencesMatch {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::ColumnConstraintReferencesMatch(Rc::new(self.clone()))
//   }
// }
// impl Visit for ColumnConstraint {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::ColumnConstraint(Rc::new(self.clone()))
//   }
// }
// impl Visit for ExprOrAll {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::ExprOrAll(Rc::new(self.clone()))
//   }
// }
// impl Visit for SelectClauseItem {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::SelectClauseItem(Rc::new(self.clone()))
//   }
// }
// impl Visit for FromClause {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::FromClause(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateFunctionArgMode {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateFunctionArgMode(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateFunctionReturns {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateFunctionReturns(Rc::new(self.clone()))
//   }
// }
// impl Visit for SqlString {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::SqlString(Rc::new(self.clone()))
//   }
// }
// impl Visit for CreateFunctionBody {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::CreateFunctionBody(Rc::new(self.clone()))
//   }
// }
// impl Visit for TransactionStmt {
//   fn children(&self) -> Vec<AstNode> {
//     unimplemented!()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::TransactionStmt(Rc::new(self.clone()))
//   }
// }
// impl Visit for Statement {
//   fn children(&self) -> Vec<AstNode> {
//     match *self {
//       Statement::CreateTable(ref s) => vec![s.ast()],
//       Statement::CreateSchema(ref s) => vec![s.ast()],
//       Statement::CreateFunction(ref s) => vec![s.ast()],
//       Statement::Select(ref s) => vec![s.ast()],
//       Statement::Transaction(ref s) => vec![s.ast()],
//     }
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::Statement(Rc::new(self.clone()))
//   }
// }
// impl Visit for Document {
//   fn children(&self) -> Vec<AstNode> {
//     self.0.iter().map(|c| c.ast()).collect()
//   }
//   fn ast(&self) -> AstNode {
//     AstNode::Document(Rc::new(self.clone()))
//   }
// }
