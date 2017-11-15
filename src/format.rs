use ast::*;
use std::sync::Arc;
use rayon;
use rayon::prelude::*;

#[derive(Debug, Clone)]
pub struct Options {
  uppercase_keywords: bool,
  escape_names: bool,
  sort_table_columns: bool,
  align_table_types: bool,
}
impl Default for Options {
  fn default() -> Options {
    Options {
      uppercase_keywords: true,
      escape_names: false,
      sort_table_columns: true,
      align_table_types: true,
    }
  }
}
#[derive(Debug, Clone)]
pub struct Context {
  options: Options,
  field_name_length: usize,
  field_type_length: usize,
  select_expr_length: usize,
}
impl Context {
  fn field_name_length(&self, len: usize) -> Context {
    Context {
      field_name_length: len,
      ..self.clone()
    }
  }
  fn field_type_length(&self, len: usize) -> Context {
    Context {
      field_type_length: len,
      ..self.clone()
    }
  }
  fn select_expr_length(&self, len: usize) -> Context {
    Context {
      select_expr_length: len,
      ..self.clone()
    }
  }
  fn keyword(&self, s: &str) -> String {
    if self.options.uppercase_keywords {
      s.to_uppercase()
    } else {
      s.to_lowercase()
    }
  }
  fn indent(&self, s: &str) -> String {
    s.lines()
      .map(|l| format!("  {}", l))
      .collect::<Vec<_>>()
      .join("\n")
  }
  fn is_builtin(&self, s: &str) -> bool {
    use keywords;
    keywords::is_keyword(s)
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
      field_name_length: 0,
      field_type_length: 0,
      select_expr_length: 0,
      options: Options::default(),
    }
  }
}

pub trait Format {
  fn format(self, ctx: &Arc<Context>) -> String;
}

fn format_join_by<T: Format + Send, I: rayon::iter::IntoParallelIterator<Item = T>>(
  items: I,
  join_by: &str,
  ctx: &Arc<Context>,
) -> String {
  items
    .into_par_iter()
    .map(|c| c.format(&ctx.clone()))
    .collect::<Vec<_>>()
    .join(join_by)
}

impl<'a> Format for &'a Var {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      Var::Raw(ref s) | &Var::Escaped(ref s) => ctx.format_name(s),
    }
  }
}
impl<'a> Format for &'a TableIdent {
  fn format(self, ctx: &Arc<Context>) -> String {
    match &self.schema {
      Some(ref schema) => format!("{}.{}", schema.format(ctx), self.name.format(ctx)),
      None => format!("{}", self.name.format(ctx)),
    }
  }
}
impl<'a> Format for &'a Ttype {
  fn format(self, ctx: &Arc<Context>) -> String {
    match &self.schema {
      Some(ref schema) => format!("{}.{}", schema.format(ctx), self.name.format(ctx)),
      None => format!("{}", self.name.format(ctx)),
    }
  }
}
impl<'a> Format for &'a FunctionCall {
  fn format(self, ctx: &Arc<Context>) -> String {
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
  fn format(self, ctx: &Arc<Context>) -> String {
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
impl<'a> Format for &'a Operator {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      Operator::And => "&&".to_owned(),
      Operator::Equal => "=".to_owned(),
      Operator::Add => "+".to_owned(),
      Operator::As => "::".to_owned(),
      Operator::AndLit => ctx.keyword("and"),
      Operator::OrLit => ctx.keyword("or"),
      Operator::Greater => ">".to_owned(),
      Operator::Less => "<".to_owned(),
      Operator::GreaterEqual => ">=".to_owned(),
      Operator::LessEqual => "<=".to_owned(),
    }
  }
}
impl<'a> Format for &'a CreateTableExcludeWith {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{} {} {}",
      self.0.format(ctx),
      ctx.keyword("with"),
      self.1.format(ctx)
    )
  }
}
impl<'a> Format for &'a CreateTableExclude {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{}({})",
      match self.using {
        Some(ref u) => format!("{} {} ", ctx.keyword("using"), u.format(ctx)),
        _ => "".to_owned(),
      },
      format_join_by(&self.with, ", ", ctx)
    )
  }
}
impl<'a> Format for &'a CreateTableField {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      CreateTableField::Column(ref c) => {
        let name_spaces = (0..ctx.field_name_length - c.name.len() + 1)
          .map(|_| " ")
          .collect::<String>();
        let type_spaces = (0..ctx.field_type_length - c.ttype.format(ctx).len() + 1)
          .map(|_| " ")
          .collect::<String>();

        match &c.constraints {
          Some(ref constraints) => format!(
            "{}{}{}{}{}",
            c.name.format(ctx),
            name_spaces,
            c.ttype.format(ctx),
            type_spaces,
            format_join_by(constraints, " ", ctx)
          ),
          None => format!(
            "{}{}{}",
            c.name.format(ctx),
            name_spaces,
            c.ttype.format(ctx)
          ),
        }
      }
      CreateTableField::Unique(ref c) => {
        let name_spaces = (0..ctx.field_name_length - "unique".len() + 1)
          .map(|_| " ")
          .collect::<String>();
        format!(
          "{}{}({})",
          ctx.keyword("unique"),
          name_spaces,
          c.format(ctx)
        )
      }
      CreateTableField::Exclude(ref e) => {
        let name_spaces = (0..ctx.field_name_length - "exclude".len() + 1)
          .map(|_| " ")
          .collect::<String>();
        format!("{}{}{}", ctx.keyword("exclude"), name_spaces, e.format(ctx))
      }
    }
  }
}
impl<'a> Format for &'a CreateTable {
  fn format(self, ctx: &Arc<Context>) -> String {
    use std;
    let mut fields: Vec<_> = self.fields.iter().map(|f| f).collect();

    if ctx.options.sort_table_columns {
      fields.sort_by(|a, b| match (&a, &b) {
        (&CreateTableField::Column(ref a), &CreateTableField::Column(ref b)) => match (
          (a.is_primary_key(), b.is_primary_key()),
          (a.is_not_null(), b.is_not_null()),
        ) {
          ((true, false), _) => std::cmp::Ordering::Less,
          ((false, true), _) => std::cmp::Ordering::Greater,
          (_, (true, false)) => std::cmp::Ordering::Less,
          (_, (false, true)) => std::cmp::Ordering::Greater,
          _ => a.name.partial_cmp(&b.name).unwrap(),
        },
        (&CreateTableField::Column(_), _) => std::cmp::Ordering::Greater,
        (_, _) => std::cmp::Ordering::Equal,
      })
    };

    let longest_field_name = fields.iter().fold(0, |max, f| {
      max.max(match f {
        CreateTableField::Column(ref f) => f.name.len(),
        CreateTableField::Unique(_) => "unique".len(),
        CreateTableField::Exclude(_) => "exclude".len(),
      })
    });
    let longest_field_type = fields.iter().fold(0, |max, f| {
      max.max(match f {
        CreateTableField::Column(ref f) => f.ttype.len(),
        CreateTableField::Unique(_) => 0,
        CreateTableField::Exclude(_) => 0,
      })
    });
    let ctx = Arc::new(
      ctx
        .field_name_length(longest_field_name)
        .field_type_length(longest_field_type),
    );

    format!(
      "{} {}\n  ( {}\n  ){}",
      ctx.keyword("create table"),
      self.name.format(&ctx),
      fields
        .iter()
        .map(|l| format!("{}", l.format(&ctx)))
        .collect::<Vec<_>>()
        .join("\n  , "),
      match self.inherits {
        Some(ref t) => format!(" {} ({})", ctx.keyword("inherits"), t.format(&ctx)),
        None => "".to_owned(),
      }
    )
  }
}
impl<'a> Format for &'a Select {
  fn format(self, ctx: &Arc<Context>) -> String {
    let longest_select_expr = self.clause.0.iter().fold(0, |max, f| {
      max.max(match f.expr_or_all {
        ExprOrAll::All => 1,
        ExprOrAll::Expression(ref expr) => expr.format(ctx).len(),
      })
    });
    let ctx = Arc::new(ctx.select_expr_length(longest_select_expr));
    format!(
      "{with}{select} {clause}{from}{where}",
      with = match self.with {
        Some(ref with) => format!("{}\n", with.format(&ctx)),
        None => "".to_owned(),
      },
      select = ctx.keyword("select"),
      clause = format_join_by(&self.clause.0, "\n     , ", &ctx),
      from = match self.from {
        Some(ref from) => format!(
          "\n  {} {}",
          ctx.keyword("from"),
          format_join_by(from, "\n     , ", &ctx)
        ),
        None => "".to_owned(),
      },
      where = match self.condition {
        Some(ref condition) => format!(
          "\n {} {}",
          ctx.keyword("where"),
          condition.format(&ctx),
        ),
        None => "".to_owned(),
      }
    )
  }
}
impl<'a> Format for &'a CreateSchema {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!("{} {}", ctx.keyword("create schema"), self.0.format(ctx))
  }
}
impl<'a> Format for &'a CreateFunctionArg {
  fn format(self, ctx: &Arc<Context>) -> String {
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
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{} {}\n  ( {}\n  )\n{} {}",
      ctx.keyword("create function"),
      self.name.format(ctx),
      format_join_by(&self.args, "\n  , ", ctx),
      format!("{} {}", ctx.keyword("returns"), self.returns.format(ctx)),
      format_join_by(&self.body, "\n", ctx)
    )
  }
}
impl<'a> Format for &'a WithItem {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{} {} (\n{}\n  )",
      self.name.format(ctx),
      ctx.keyword("as"),
      ctx.indent(&ctx.indent(&self.expr.format(ctx)))
    )
  }
}
impl<'a> Format for &'a With {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{}\n  {}",
      ctx.keyword("with"),
      format_join_by(&self.0, ",\n  ", ctx)
    )
  }
}
impl<'a> Format for &'a Cast {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{}({} {} {})",
      ctx.keyword("cast"),
      self.0.format(ctx),
      ctx.keyword("as"),
      self.1.format(ctx)
    )
  }
}
impl<'a> Format for &'a Number {
  fn format(self, _ctx: &Arc<Context>) -> String {
    match self {
      Number::Float(ref f) => format!("{}", f),
      Number::Int(ref f) => format!("{}", f),
    }
  }
}
impl<'a> Format for &'a Expression {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      Expression::Infix(ref opr, ref exprs) => format!(
        "{} {} {}",
        exprs.0.format(ctx),
        opr.format(ctx),
        exprs.1.format(ctx)
      ),
      Expression::FunctionCall(ref fncall) => fncall.format(ctx),
      Expression::Ref(ref r) => r.format(ctx),
      Expression::Insert(ref i) => i.format(ctx),
      Expression::String(ref s) => s.format(ctx),
      Expression::Number(ref n) => n.format(ctx),
      Expression::Select(ref sel) => sel.format(ctx),
      Expression::Cast(ref cast) => cast.format(ctx),
    }
  }
}
impl<'a> Format for &'a ExprOrDefault {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      ExprOrDefault::Default => ctx.keyword("default"),
      ExprOrDefault::Expression(ref expr) => expr.format(ctx),
    }
  }
}
impl<'a> Format for &'a InsertValues {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      InsertValues::Default => format!("{}", ctx.keyword("default")),
      InsertValues::Values(ref vs) => format!(
        "{}\n  ({})",
        ctx.keyword("values"),
        format_join_by(vs, ", ", ctx)
      ),
      InsertValues::Select(ref sel) => format!("{}", sel.format(ctx)),
    }
  }
}
impl<'a> Format for &'a InsertConflictUpdate {
  fn format(self, _ctx: &Arc<Context>) -> String {
    format!("") // TODO
  }
}
impl<'a> Format for &'a InsertConflictAction {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      InsertConflictAction::Nothing => ctx.keyword("nothing"),
      InsertConflictAction::Update(ref u) => format!("{} {}", ctx.keyword("update"), u.format(ctx)),
    }
  }
}
impl<'a> Format for &'a InsertConflict {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{} ({}) {}",
      ctx.keyword("on conflict"),
      format_join_by(&self.target, ", ", ctx),
      self.action.format(ctx)
    )
  }
}
impl<'a> Format for &'a InsertReturn {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      InsertReturn::All => format!("{} *", ctx.keyword("returning")),
    }
  }
}
impl<'a> Format for &'a InsertStmt {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{with}{kw} {table}\n{columns}\n{values}{conflict}{ret}",
      with = match self.with {
        Some(ref w) => format!("{}\n", w.format(ctx)),
        None => format!(""),
      },
      kw = ctx.keyword("insert into"),
      table = self.table.format(ctx),
      columns = ctx.indent(&match self.columns {
        Some(ref columns) => format!("({})", format_join_by(columns, ", ", ctx)),
        None => format!(""),
      }),
      values = &self.values.format(ctx),
      conflict = match self.conflict {
        Some(ref c) => ctx.indent(&format!("\n{}", c.format(ctx))),
        None => "".to_owned(),
      },
      ret = match self.ret {
        Some(ref c) => format!("\n{}", c.format(ctx)),
        None => "".to_owned(),
      },
    )
  }
}
impl<'a> Format for &'a ColumnConstraintReferencesMatch {
  fn format(self, ctx: &Arc<Context>) -> String {
    use ast::ColumnConstraintReferencesMatch;
    match self {
      ColumnConstraintReferencesMatch::Full => ctx.keyword("full"),
      ColumnConstraintReferencesMatch::Partial => ctx.keyword("partial"),
      ColumnConstraintReferencesMatch::Simple => ctx.keyword("simple"),
    }
  }
}
impl<'a> Format for &'a ColumnConstraint {
  fn format(self, ctx: &Arc<Context>) -> String {
    use ast::ColumnConstraint::*;
    match self {
      NotNull => ctx.keyword("not null"),
      Null => unimplemented!(),
      Check => unimplemented!(),
      Default(ref expr) => format!("{} {}", ctx.keyword("default"), expr.format(ctx)),
      Unique => ctx.keyword("unique"),
      PrimaryKey => ctx.keyword("primary key"),
      References(ref re) => re.format(ctx),
    }
  }
}
impl<'a> Format for &'a ExprOrAll {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      ExprOrAll::All => "*".to_owned(),
      ExprOrAll::Expression(ref expr) => expr.format(ctx),
    }
  }
}
impl<'a> Format for &'a SelectClauseItem {
  fn format(self, ctx: &Arc<Context>) -> String {
    let expr = self.expr_or_all.format(ctx);
    let as_spaces = (0..ctx.select_expr_length - expr.len() + 1)
      .map(|_| " ")
      .collect::<String>();
    format!(
      "{}{}",
      expr,
      match self.ass {
        Some(ref ass) => format!("{}{} {}", as_spaces, ctx.keyword("as"), ass.format(ctx)),
        None => "".to_owned(),
      }
    )
  }
}
impl<'a> Format for &'a FromClause {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{expr}{ass}",
      expr = self.from.format(ctx),
      ass = match self.ass {
        Some(ref ass) => format!(" {} {}", ctx.keyword("as"), ass.format(ctx)),
        None => "".to_owned(),
      },
    )
  }
}
impl<'a> Format for &'a CreateFunctionArgMode {
  fn format(self, _ctx: &Arc<Context>) -> String {
    unimplemented!()
  }
}
impl<'a> Format for &'a CreateFunctionReturns {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      CreateFunctionReturns::Table(ref create_table) => create_table.format(ctx),
      CreateFunctionReturns::Ttype(ref ttype) => ttype.format(ctx),
      CreateFunctionReturns::SetOf(ref ttype) => {
        format!("{} {}", ctx.keyword("setof"), ttype.format(ctx))
      }
    }
  }
}
impl<'a> Format for &'a SqlString {
  fn format(self, _ctx: &Arc<Context>) -> String {
    match self {
      SqlString::Raw(ref s) => format!("'{}'", s),
      SqlString::DollarQuoted(ref p, ref s) => format!("{}{}{}", p, s, p),
    }
  }
}
impl<'a> Format for &'a CreateFunctionBody {
  fn format(self, ctx: &Arc<Context>) -> String {
    use ast::CreateFunctionBody::*;
    match self {
      AsDef(_) => unreachable!(),
      ParsedAsDef(ref doc) => format!(
        "{} $$\n{}\n$$",
        ctx.keyword("as"),
        ctx.indent(&doc.format(ctx))
      ),
      Language(ref var) => format!("{} {}", ctx.keyword("language"), var.format(ctx)),
    }
  }
}
impl<'a> Format for &'a TransactionStmt {
  fn format(self, ctx: &Arc<Context>) -> String {
    match self {
      TransactionStmt::Begin => ctx.keyword("begin"),
      TransactionStmt::End => ctx.keyword("end"),
    }
  }
}
impl<'a> Format for &'a View {
  fn format(self, ctx: &Arc<Context>) -> String {
    format!(
      "{} {} {}\n{}",
      ctx.keyword("create view"),
      self.name.format(ctx),
      ctx.keyword("as"),
      ctx.indent(&self.ass.format(ctx))
    )
  }
}
impl<'a> Format for &'a Statement {
  fn format(self, ctx: &Arc<Context>) -> String {
    use ast::Statement::*;
    match self {
      CreateTable(ref stmt) => stmt.format(ctx),
      CreateSchema(ref stmt) => stmt.format(ctx),
      CreateFunction(ref stmt) => stmt.format(ctx),
      Select(ref stmt) => stmt.format(ctx),
      Transaction(ref stmt) => stmt.format(ctx),
      Return(ref ret) => ret.format(ctx),
      Insert(ref i) => i.format(ctx),
      View(ref v) => v.format(ctx),
    }
  }
}
impl<'a> Format for &'a ReturnStmt {
  fn format(self, ctx: &Arc<Context>) -> String {
    use ast::ReturnStmt::*;
    format!(
      "{} {}",
      ctx.keyword("return"),
      match self {
        Query(ref query) => format!(
          "{}\n{}",
          ctx.keyword("query"),
          ctx.indent(&query.format(ctx))
        ),
        Expression(ref expr) => expr.format(ctx),
      }
    )
  }
}
impl<'a> Format for &'a Document {
  fn format(self, ctx: &Arc<Context>) -> String {
    let mut s: String = self
      .0
      .iter()
      .map(|s| format!("{};\n\n", s.format(ctx)))
      .collect();
    s.pop();
    s
  }
}
