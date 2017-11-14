pub struct Options {
  uppercase_keywords: bool,
  escape_names: bool,
  sort_table_columns: bool,
}
impl Default for Options {
  fn default() -> Options {
    Options {
      uppercase_keywords: true,
      escape_names: false,
      sort_table_columns: true,
    }
  }
}
pub struct Context {
  options: Options,
}
impl Context {
  fn keyword(&self, s: &str) -> String {
    if self.options.uppercase_keywords {
      s.to_uppercase()
    } else {
      s.to_lowercase()
    }
  }
  fn is_builtin(&self, s: &str) -> bool {
    match &*s.to_lowercase() {
      "uuid" | "text" | "timestamp" | "now" | "uuid_generate_v4" => true,
      _ => false,
    }
  }
  fn format_builtin(&self, s: &str) -> String {
    if self.is_builtin(s) {
      self.keyword(s)
    } else {
      s.to_owned()
    }
  }
  fn format_name(&self, s: &str) -> String {
    if self.is_builtin(s) {
      self.format_builtin(s)
    } else if self.options.escape_names {
      format!("`{}`", s)
    } else {
      s.to_owned()
    }
  }
  pub fn uppercase_keywords(&self) -> Context {
    Context {
      options: Options {
        uppercase_keywords: true,
        ..self.options
      },
      ..*self
    }
  }
  pub fn lowercase_keywords(&self) -> Context {
    Context {
      options: Options {
        uppercase_keywords: false,
        ..self.options
      },
      ..*self
    }
  }
}
impl Default for Context {
  fn default() -> Context {
    Context {
      options: Options::default(),
    }
  }
}

pub trait Format {
  fn format(self, ctx: &Context) -> String;
}

fn format_join_by<T: Format, I: IntoIterator<Item = T>>(
  items: I,
  join_by: &str,
  ctx: &Context,
) -> String {
  items
    .into_iter()
    .map(|c| c.format(ctx))
    .collect::<Vec<_>>()
    .join(join_by)
}

use parser::*;

impl<'a> Format for &'a Var {
  fn format(self, ctx: &Context) -> String {
    match self {
      &Var::Raw(ref s) | &Var::Escaped(ref s) => ctx.format_name(s),
    }
  }
}
impl<'a> Format for &'a TableIdent {
  fn format(self, ctx: &Context) -> String {
    match &self.schema {
      &Some(ref schema) => format!("{}.{}", schema.format(ctx), self.name.format(ctx)),
      &None => format!("{}", self.name.format(ctx)),
    }
  }
}
impl<'a> Format for &'a Ttype {
  fn format(self, ctx: &Context) -> String {
    match &self.schema {
      &Some(ref schema) => format!("{}.{}", schema.format(ctx), self.name.format(ctx)),
      &None => format!("{}", self.name.format(ctx)),
    }
  }
}
impl<'a> Format for &'a FunctionCall {
  fn format(self, ctx: &Context) -> String {
    match self.args {
      Some(ref args) => format!(
        "{}({})",
        self.base.format(ctx),
        format_join_by(args, ", ", ctx)
      ),
      None => format!("{}()", self.base.format(ctx)),
    }
  }
}
impl<'a> Format for &'a ColumnConstraintReferences {
  fn format(self, ctx: &Context) -> String {
    format!(
      "{} {}{}{}",
      ctx.keyword("references"),
      self.table.format(ctx),
      match self.column {
        Some(ref c) => format!(" ({})", c.format(ctx)),
        None => "".to_owned(),
      },
      match self.mmatch {
        Some(ref m) => format!(" {} {}", ctx.keyword("match"), m.format(ctx)),
        None => "".to_owned(),
      }
    )
  }
}
impl<'a> Format for &'a CreateTableField {
  fn format(self, ctx: &Context) -> String {
    match &self.constraints {
      &Some(ref constraints) => format!(
        "{} {} {}",
        self.name.format(ctx),
        self.ttype.format(ctx),
        format_join_by(constraints, " ", ctx)
      ),
      &None => format!("{} {}", self.name.format(ctx), self.ttype.format(ctx)),
    }
  }
}
impl<'a> Format for &'a CreateTable {
  fn format(self, ctx: &Context) -> String {
    let mut fields: Vec<_> = self.fields.iter().map(|f| f).collect();

    if ctx.options.sort_table_columns {
      fields.sort_by(|a, b| a.name.partial_cmp(&b.name).unwrap())
    };

    format!(
      "{} {} (\n{}\n)",
      ctx.keyword("create table"),
      self.name.format(ctx),
      fields
        .iter()
        .map(|l| format!("  {}", l.format(ctx)))
        .collect::<Vec<_>>()
        .join(",\n")
    )
  }
}
impl<'a> Format for &'a SelectClause {
  fn format(self, ctx: &Context) -> String {
    format_join_by(&self.0, ", ", ctx)
  }
}
impl<'a> Format for &'a Select {
  fn format(self, ctx: &Context) -> String {
    format!(
      "{} {} {}",
      ctx.keyword("select"),
      self.clause.format(ctx),
      self.from.format(ctx)
    )
  }
}
impl<'a> Format for &'a CreateSchema {
  fn format(self, ctx: &Context) -> String {
    format!("{} {}", ctx.keyword("create schema"), self.0.format(ctx))
  }
}
impl<'a> Format for &'a CreateFunctionArg {
  fn format(self, ctx: &Context) -> String {
    format!(
      "{}{}{}{}",
      match self.mode {
        Some(ref m) => format!("{} ", m.format(ctx)),
        None => "".to_owned(),
      },
      match self.name {
        Some(ref m) => format!("{} ", m.format(ctx)),
        None => "".to_owned(),
      },
      self.ttype.format(ctx),
      match self.default {
        Some(ref m) => format!(" {}", m.format(ctx)),
        None => "".to_owned(),
      },
    )
  }
}
impl<'a> Format for &'a CreateFunction {
  fn format(self, ctx: &Context) -> String {
    format!(
      "{} {}({}) {} {}",
      ctx.keyword("create function"),
      self.name.format(ctx),
      format_join_by(&self.args, ", ", ctx),
      format!("{} {}", ctx.keyword("returns"), self.returns.format(ctx)),
      format_join_by(&self.body, "\n", ctx)
    )
  }
}
impl<'a> Format for &'a Expression {
  fn format(self, ctx: &Context) -> String {
    match self {
      &Expression::FunctionCall(ref fncall) => fncall.format(ctx),
    }
  }
}
impl<'a> Format for &'a ColumnConstraintReferencesMatch {
  fn format(self, ctx: &Context) -> String {
    use parser::ColumnConstraintReferencesMatch::*;
    match self {
      &Full => ctx.keyword("full"),
      &Partial => ctx.keyword("partial"),
      &Simple => ctx.keyword("simple"),
    }
  }
}
impl<'a> Format for &'a ColumnConstraint {
  fn format(self, ctx: &Context) -> String {
    use parser::ColumnConstraint::*;
    match self {
      &NotNull => ctx.keyword("not null"),
      &Null => unimplemented!(),
      &Check => unimplemented!(),
      &Default(ref expr) => format!("{} {}", ctx.keyword("default"), expr.format(ctx)),
      &Unique => ctx.keyword("unique"),
      &PrimaryKey => ctx.keyword("primary key"),
      &References(ref re) => re.format(ctx),
    }
  }
}
impl<'a> Format for &'a ExprOrAll {
  fn format(self, ctx: &Context) -> String {
    match self {
      &ExprOrAll::All => "*".to_owned(),
      &ExprOrAll::Expression(ref expr) => expr.format(ctx),
    }
  }
}
impl<'a> Format for &'a SelectClauseItem {
  fn format(self, ctx: &Context) -> String {
    format!(
      "{}{}",
      self.expr_or_all.format(ctx),
      match self.ass {
        Some(ref ass) => format!(" {} {}", ctx.keyword("as"), ass),
        None => "".to_owned(),
      }
    )
  }
}
impl<'a> Format for &'a FromClause {
  fn format(self, ctx: &Context) -> String {
    format!(
      "{} {}",
      ctx.keyword("from"),
      match self {
        &FromClause::Table(ref tid) => tid.format(ctx),
      }
    )
  }
}
impl<'a> Format for &'a CreateFunctionArgMode {
  fn format(self, _ctx: &Context) -> String {
    unimplemented!()
  }
}
impl<'a> Format for &'a CreateFunctionReturns {
  fn format(self, ctx: &Context) -> String {
    match self {
      &CreateFunctionReturns::Table(ref create_table) => create_table.format(ctx),
      &CreateFunctionReturns::Ttype(ref ttype) => ttype.format(ctx),
      &CreateFunctionReturns::SetOf(ref ttype) => {
        format!("{} {}", ctx.keyword("setof"), ttype.format(ctx))
      }
    }
  }
}
impl<'a> Format for &'a SqlString {
  fn format(self, _ctx: &Context) -> String {
    match self {
      &SqlString::Raw(ref s) => format!("'{}'", s),
      &SqlString::DollarQuoted(ref p, ref s) => format!("{}{}{}", p, s, p),
    }
  }
}
impl<'a> Format for &'a CreateFunctionBody {
  fn format(self, ctx: &Context) -> String {
    use parser::CreateFunctionBody::*;
    match self {
      &AsDef(_) => unimplemented!(),
      &ParsedAsDef(ref doc) => format!(
        "{} $$\n{}\n$$",
        ctx.keyword("as"),
        doc
          .format(ctx)
          .lines()
          .map(|l| format!("  {}", l))
          .collect::<Vec<_>>()
          .join("\n")
      ),
      &Language(ref var) => format!("{} {}", ctx.keyword("language"), var.format(ctx)),
    }
  }
}
impl<'a> Format for &'a TransactionStmt {
  fn format(self, ctx: &Context) -> String {
    match self {
      &TransactionStmt::Begin => ctx.keyword("begin"),
      &TransactionStmt::End => ctx.keyword("end"),
    }
  }
}
impl<'a> Format for &'a Statement {
  fn format(self, ctx: &Context) -> String {
    use parser::Statement::*;
    match self {
      &CreateTable(ref stmt) => stmt.format(ctx),
      &CreateSchema(ref stmt) => stmt.format(ctx),
      &CreateFunction(ref stmt) => stmt.format(ctx),
      &Select(ref stmt) => stmt.format(ctx),
      &Transaction(ref stmt) => stmt.format(ctx),
    }
  }
}
impl<'a> Format for &'a Document {
  fn format(self, ctx: &Context) -> String {
    self
      .0
      .iter()
      .map(|s| format!("{};\n", s.format(ctx)))
      .collect()
  }
}
