use pest;
use pest::Parser;

#[derive(Parser)]
#[grammar = "sql.pest"]
pub struct IdentParser;

use ast;
use ast::*;

#[derive(Debug, Clone)]
pub enum ParseError {
  Dunno,
  Comment,
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
    // Ok(self.next().ok_or(ParseError::Dunno).unwrap())
  }
  fn pop_comments(&mut self) -> Result<P<'a>, ParseError> {
    self.pop().and_then(|t| match t.as_rule() {
      Rule::comment => self.pop(),
      _ => Ok(t)
    })
  }
}

impl<'a, T> Ps<'a> for T
where
  T: Iterator<Item = P<'a>>,
{
}

macro_rules! unparseable {
    ($fmt:expr) => ({
        unreachable!("{:?} : {}:{}", $fmt, file!(), line!())
        // return Err(ParseError::Unparseable(format!("{:?} : {}:{} -> {}", $fmt, file!(), line!(), $fmt.as_str())))
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

trait ParseOne: Sized {
  fn parse_one<'a>(t: P<'a>) -> Result<Self, ParseError>;
}

impl<T> Parseable for T where T: ParseOne {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Self, ParseError> {
    let t = inp.pop()?;
    Self::parse_one(t)
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
          name: {
            let t = inp.pop()?;
            match t.as_rule() {
              Rule::s_var => Var::parse(t.into_inner())?,
              Rule::all => Var::Raw("*".to_owned()), // TODO
              _ => unparseable!(t)
            }
          },
        }
      }
      _ => unparseable!(t),
    })
  }
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

impl Parseable for Cast {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Cast, ParseError> {
    let t = inp.pop()?;
    let expr = match t.as_rule() {
      Rule::expr => Expression::parse(t.into_inner())?,
      _ => unparseable!(t),
    };
    let t = inp.pop()?;
    let ttype = match t.as_rule() {
      Rule::ttype => Ttype::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    Ok(Cast(expr, ttype))
  }
}

impl ParseOne for Number {
  fn parse_one<'a>(t: P<'a>) -> Result<Number, ParseError> {
    Ok(match t.as_rule() {
      Rule::float => Number::Float(t.as_str().parse().unwrap()),
      Rule::int => Number::Int(t.as_str().parse().unwrap()),
      _ => unparseable!(t)
    })
  }
}

impl Parseable for Expression {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Expression, ParseError> {
    let t = inp.pop()?;
    let expr = match t.as_rule() {
      Rule::expr | Rule::term | Rule::equality | Rule::addative | Rule::primary | Rule::binding => {
        Expression::parse(t.into_inner())
      }
      Rule::number => {
        Number::parse(t.into_inner()).map(|c| Expression::Number(c))
      }
      Rule::cast => {
        Cast::parse(t.into_inner()).map(|c| Expression::Cast(Box::new(c)))
      }
      Rule::string => {
        SqlString::parse(t.into_inner()).map(|c| Expression::String(c))
      }
      Rule::function_call => {
        FunctionCall::parse(t.into_inner()).map(|c| Expression::FunctionCall(c))
      }
      Rule::table_ident => {
        TableIdent::parse(t.into_inner()).map(|c| Expression::Ref(c))
      }
      Rule::select_stmt => {
        Select::parse(t.into_inner()).map(|c| Expression::Select(Box::new(c)))
      }
      Rule::insert_stmt => {
        InsertStmt::parse(t.into_inner()).map(|c| Expression::Insert(c))
      }
      _ => unparseable!(t)
    }?;

    inp.fold(Ok((expr, None)), |z, t| {
      let (expr, operator) = z?;
      Ok(match t.as_rule() {
        Rule::comment => (expr, operator),
        Rule::as_infix => {
          let mut inp = t.into_inner();
          let t = inp.pop()?;
          match t.as_rule() {
            Rule::ttype => (Expression::Cast(Box::new(Cast(expr, Ttype::parse(t.into_inner())?))), operator),
            _ => unparseable!(t)
          }
        },
        Rule::expr | Rule::term | Rule::equality | Rule::addative | Rule::primary | Rule::binding => {
          (Expression::Infix(operator.unwrap(), Box::new((expr, Expression::parse(t.into_inner())?))), None)
        },
        _ => {
          match Operator::parse_one(t) {
            Ok(opr) => (expr, Some(opr)),
            Err(e) => return Err(e)
          }
        }
      })
    }).map(|(expr, _)| expr)
  }
}

impl Parseable for WithItem {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<WithItem, ParseError> {
    let t = inp.pop()?;
    let name = match t.as_rule() {
      Rule::s_var => Var::parse(t.into_inner()),
      _ => unparseable!(t)
    }?;

    let t = inp.pop()?;
    let expr = match t.as_rule() {
      Rule::expr => Expression::parse(t.into_inner()),
      _ => unparseable!(t),
    }?;

    Ok(WithItem {
      name: name,
      expr: expr
    })
  }
}

impl Parseable for With {
  fn parse<'a, I: Ps<'a>>(inp: I) -> Result<With, ParseError> {
    let xs = a(inp.map(|t| match t.as_rule() {
      Rule::with_item => WithItem::parse(t.into_inner()),
      _ => unparseable!(t),
    }))?;

    Ok(ast::With(xs))
  }
}

impl Parseable for InsertConflictUpdate {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<InsertConflictUpdate, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::insert_conflict_action_update_item => unimplemented!(), // TODO
      _ => unparseable!(t)
    })
  }}

impl Parseable for InsertConflictAction {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<InsertConflictAction, ParseError> {
    let t = inp.pop()?;
    Ok(match t.as_rule() {
      Rule::insert_conflict_action_update => InsertConflictAction::Update(InsertConflictUpdate::parse(t.into_inner())?),
      Rule::insert_conflict_action_nothing => InsertConflictAction::Nothing,
      _ => unparseable!(t)
    })
  }}
impl Parseable for InsertConflict {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<InsertConflict, ParseError> {
    let t = inp.pop()?;
    let target = match t.as_rule() {
      Rule::insert_conflict_target => a(t.into_inner().map(|t| Var::parse(t.into_inner())))?,
      _ => unparseable!(t)
    };

    let t = inp.pop()?;
    let action = match t.as_rule() {
      Rule::insert_conflict_action => InsertConflictAction::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    Ok(InsertConflict {
      target: target,
      action: action,
    })
  }
}

impl ParseOne for ExprOrDefault {
  fn parse_one<'a>(t: P<'a>) -> Result<ExprOrDefault, ParseError> {
    Ok(match t.as_rule() {
      Rule::expr => ExprOrDefault::Expression(Expression::parse(t.into_inner())?),
      Rule::default_lit => ExprOrDefault::Default,
      _ => unparseable!(t)
    })
  }
}

impl Parseable for InsertStmt {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<InsertStmt, ParseError> {
    // with: Option<With>,
    // table: TableIdent,
    // columns: Option<Vec<Var>>,
    // values: InsertValues,
    // conflict: Option<InsertConflict>,
    // ret: Option<InsertReturn>,

    let t = inp.pop()?;
    let (t, with) = match t.as_rule() {
      Rule::with => (inp.pop()?, Some(With::parse(t.into_inner())?)),
      Rule::table_ident => (t, None),
      _ => unparseable!(t),
    };

    let table = match t.as_rule() {
      Rule::table_ident => TableIdent::parse(t.into_inner())?,
      _ => unparseable!(t)
    };

    let t = inp.pop()?;
    let (t, columns) = match t.as_rule() {
      Rule::insert_columns => {
        let mut old_inp = &mut inp;
        let mut inp = t.into_inner();

        (old_inp.pop()?, Some(a(inp.map(|t| match t.as_rule() {
          Rule::s_var => Var::parse(t.into_inner()),
          _ => unparseable!(t)
        }))?))
      },
      _ => unparseable!(t),
    };

    let values = match t.as_rule() {
      Rule::select_stmt => InsertValues::Select(Box::new(Select::parse(t.into_inner())?)),
      Rule::insert_values => InsertValues::Values(a(t.into_inner().map(|t| match t.as_rule() {
        Rule::insert_values_item => ExprOrDefault::parse(t.into_inner()),
        _ => unparseable!(t)
      }))?),
      _  => unparseable!(t)
    };

    let (conflict, ret) = match inp.next() {
      Some(t) => {
    let (t, conflict) = match t.as_rule() {
      Rule::insert_conflict => (inp.pop()?, Some(InsertConflict::parse(t.into_inner())?)),
      Rule::insert_return => (t, None),
      _  => unparseable!(t)
    };

    let ret = match t.as_rule() {
      Rule::insert_return => Some(InsertReturn::All),
      _ => None,
    };
    (conflict, ret)},
    None => (None, None)
    };

    Ok(InsertStmt {
      with: with,
      table: table,
      columns: columns,
      values: values,
      conflict: conflict,
      ret: ret,
    })
  }
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
    let (_t, mmatch) = match t {
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

impl ParseOne for ColumnConstraint {
  fn parse_one<'a>(t: P<'a>) -> Result<ColumnConstraint, ParseError> {
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

impl ParseOne for Operator {
  fn parse_one<'a>(t: P<'a>) -> Result<Operator, ParseError> {
    Ok(match t.as_rule() {
      Rule::operator => Operator::parse(t.into_inner())?,
      Rule::operator_eq => Operator::Equal,
      Rule::operator_and => Operator::And,
      Rule::operator_add => Operator::Add,
      Rule::or_lit => Operator::OrLit,
      Rule::and_lit => Operator::AndLit,
      Rule::operator_gt => Operator::Greater,
      Rule::operator_lt => Operator::Less,
      Rule::operator_gte => Operator::GreaterEqual,
      Rule::operator_lte => Operator::LessEqual,
      Rule::operator_contains => Operator::Contains,
      Rule::operator_contained_by => Operator::ContainedBy,
      Rule::comment => return Err(ParseError::Comment),
      _ => unparseable!(t)
    })
  }
}

impl Parseable for CreateTableExcludeWith {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateTableExcludeWith, ParseError> {
    let t = inp.pop()?;
    let expr = match t.as_rule() {
      Rule::column_exclude_elm => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        match t.as_rule() {
          Rule::expr => Expression::parse(t.into_inner())?,
          _ => unparseable!(t),
        }
      }
      _ => unparseable!(t)
    };

    let t = inp.pop()?;
    let operator = match t.as_rule() {
      Rule::operator => Operator::parse(t.into_inner())?,
      _ => unparseable!(t)
    };

    Ok(CreateTableExcludeWith(expr, operator))
  }
}

impl Parseable for CreateTableExclude {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateTableExclude, ParseError> {
    let t = inp.pop()?;
    let (t, using) = match t.as_rule() {
      Rule::column_exclude_using => {
        let mut ninp = t.into_inner();
        let t = ninp.pop()?;
        match t.as_rule() {
          Rule::table_ident => (inp.pop()?, Some(TableIdent::parse(t.into_inner())?)),
          _ => unparseable!(t)
        }
      },
      _ => unparseable!(t)
    };

    let with = match t.as_rule() {
      Rule::column_exclude_list => a(t.into_inner().map(|t| match t.as_rule() {
        Rule::column_exclude_item => CreateTableExcludeWith::parse(t.into_inner()),
        _ => unparseable!(t)
      }))?,
      _ => unparseable!(t)
    };

    Ok(CreateTableExclude {
      using: using,
      with: with,
    })
  }
}

impl Parseable for CreateTableField {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<CreateTableField, ParseError> {
    let t = inp.pop()?;
    match t.as_rule() {
      Rule::create_table_column => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        let name = match t.as_rule() {
          // Rule::s_var => Var::parse(t.into_inner())?,
          Rule::s_var => Var::parse(t.into_inner())?,
          _ => unparseable!(t),
        };


        let t = inp.pop()?;
        let ttype = match t.as_rule() {
          Rule::ttype => Ttype::parse(t.into_inner())?,
          _ => unparseable!(t),
        };

        // TODO COLLATE

        let constraints = match inp.next().and_then(|t| match t.as_rule() {
          Rule::column_constraints => Some(a(t.into_inner().map(|t| match t.as_rule() {
            Rule::column_constraint => ColumnConstraint::parse(t.into_inner()),
            _ => unparseable!(t),
          }))),
          Rule::comment => None,
          _ => unreachable!("{:?}", t),
        }) {
          Some(r) => Some(r?),
          None => None,
        };

        Ok(CreateTableField::Column(CreateTableColumn {
          name: name,
          ttype: ttype,
          constraints: constraints,
        }))
      }
      Rule::create_table_unique => {
        let mut inp = t.into_inner();
        let t = inp.pop()?;
        let column = match t.as_rule() {
          Rule::s_var => Var::parse(t.into_inner())?,
          _ => unparseable!(t),
        };

        Ok(CreateTableField::Unique(column))
      }
      Rule::column_exclude => {
        Ok(CreateTableField::Exclude(CreateTableExclude::parse(t.into_inner())?))
      }
      _ => unparseable!(t)
    }
  }
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

    let t = inp.pop()?;
    let fields = match t.as_rule() {
      Rule::create_table_body => {
        let inp = t.into_inner();
        a(inp.filter_map(|t| {
          match t.as_rule() {
            Rule::create_table_item => Some(CreateTableField::parse(t.into_inner())),
            Rule::ttype => unimplemented!(),
            Rule::comment => None,
            _ => unreachable!("{:?}", t)
          }
        }))?
      },
      _ => unparseable!(t)
    };

    let inherits = inp.next().map(|t| match t.as_rule() {
        Rule::create_table_inherits => {
          let mut inp = t.into_inner();
          let t = inp.pop()?;

          match t.as_rule() {
            Rule::table_ident => TableIdent::parse(t.into_inner()),
            _ => unparseable!(t)
          }
        },
        _ => unparseable!(t)
      }
    );

    Ok(CreateTable {
      name: name,
      if_not_exsists: if_not_exsists,
      fields: fields,
      inherits: match inherits {
        Some(i) => Some(i?),
        None => None,
      },
    })
  }
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
        Rule::s_var => Some(Var::parse(t.into_inner())?),
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

impl Parseable for SelectClause {
  fn parse<'a, I: Ps<'a>>(inp: I) -> Result<SelectClause, ParseError> {
    Ok(SelectClause(
      a(inp.map(|t| SelectClauseItem::parse(t.into_inner())))?,
    ))
  }
}

impl Parseable for FromClause {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<FromClause, ParseError> {
    let t = inp.pop()?;
    let expr = match t.as_rule() {
      Rule::expr => Expression::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    let t = inp.next();
    let ass = match t {
      Some(t) => match t.as_rule() {
        Rule::s_var => Some(Var::parse(t.into_inner())?),
        _ => unparseable!(t),
      },
      None => None
    };

    Ok(FromClause {
      from: expr,
      ass: ass,
    })
  }
}

impl Parseable for Select {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Select, ParseError> {
    let t = inp.pop()?;
    let (t, with) = match t.as_rule() {
      Rule::with => (inp.pop()?, Some(With::parse(t.into_inner())?)),
      Rule::select_clause => (t, None),
      _ => unparseable!(t),
    };

    let select_clause = match t.as_rule() {
      Rule::select_clause => SelectClause::parse(t.into_inner())?,
      _ => unparseable!(t),
    };

    let t = inp.next();
    let (t, from_clause) = match t {
      Some(t) => (inp.next(), Some(match t.as_rule() {
        Rule::select_from => a(t.into_inner().filter_map(|t| match t.as_rule() {
          Rule::select_from_item => Some(FromClause::parse(t.into_inner())),
          Rule::comment => None,
          _ => unreachable!("{:?}", t),
        }))?,
        _ => unparseable!(t),
      })),
      None => (t, None)
    };

    let (t, condition) = match t {
      Some(tt) => match tt.as_rule() {
        Rule::select_where => {
          let mut old_inp = &mut inp;
          let mut inp = tt.into_inner();
          let t = inp.pop_comments()?;

          match t.as_rule() {
            Rule::expr => (old_inp.next(), Some(Expression::parse(t.into_inner())?)),
            _ => unparseable!(t)
          }
        },
        Rule::select_limit | Rule::comment => (Some(tt), None),
        _ => unparseable!(tt),
      },
      None => (t, None)
  };

    let (t, limit) = match t {
      Some(tt) => match tt.as_rule() {
        Rule::select_limit => {
          let mut old_inp = &mut inp;
          let mut inp = tt.into_inner();
          let t = inp.pop()?;

          match t.as_rule() {
            Rule::all_lit => (old_inp.next(), Some(Limit::All)),
            Rule::expr => (old_inp.next(), Some(Limit::Count(Expression::parse(t.into_inner())?))),
            _ => unparseable!(t)
          }
        },
        Rule::comment => (Some(tt), None),
        _ => unparseable!(tt),
      },
      None => (t, None)
  };

    Ok(Select {
      with: with,
      clause: select_clause,
      from: from_clause,
      condition: condition,
      limit: limit,
    })
  }
}

impl ParseOne for CreateSchema {
  fn parse_one<'a>(t: P<'a>) -> Result<CreateSchema, ParseError> {
    match t.as_rule() {
      Rule::s_var => Ok(CreateSchema(Var::parse(t.into_inner())?)),
      _ => unparseable!(t)
    }
  }
}

impl Parseable for CreateFunctionArgMode {
  fn parse<'a, I: Ps<'a>>(_inp: I) -> Result<CreateFunctionArgMode, ParseError> {
    unimplemented!()
  }
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

impl ParseOne for CreateFunctionReturns {
  fn parse_one<'a>(t: P<'a>) -> Result<CreateFunctionReturns, ParseError> {
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
    let t = inp.next();

    match t {
      None => Ok(SqlString::Raw("".to_owned())), // TODO
      Some(t) => match t.as_rule() {
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
            Rule::comment => unreachable!("{}", t.as_str()),
            _ => unparseable!(t),
          };
          Ok(SqlString::DollarQuoted(p, contents))
        }
        Rule::single_quoted => {
          let s = t.as_str();
          Ok(SqlString::Raw((&s[1..s.len()-1]).to_owned()))
        }
        _ => unparseable!(t),
      }
    }
  }
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

            match parse(&s) {
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

impl Parseable for TransactionStmt {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<TransactionStmt, ParseError> {
    let t = inp.pop()?;
    Ok(rule!(t, {
      begin_stmt => TransactionStmt::Begin
      end_stmt => TransactionStmt::End
    }))
  }
}

impl Parseable for ReturnStmt {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<ReturnStmt, ParseError> {
    let t = inp.pop()?;
    rule!(t, {
      expr => Expression::parse(t.into_inner()).map(|c| ReturnStmt::Expression(c))
      ret_query => Expression::parse(t.into_inner().pop()?.into_inner()).map(|c| ReturnStmt::Query(c))
    })
  }
}

impl Parseable for View {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<View, ParseError> {
    let t = inp.pop()?;
    let name = match t.as_rule() {
      Rule::table_ident => TableIdent::parse(t.into_inner())?,
      _ => unparseable!(t),
    };
    let t = inp.pop()?;
    let ass = match t.as_rule() {
      Rule::select_stmt => Select::parse(t.into_inner())?,
      _ => unparseable!(t),
    };
    
    Ok(View {
      name: name,
      ass: ass
    })
  }
}

impl Parseable for Statement {
  fn parse<'a, I: Ps<'a>>(mut inp: I) -> Result<Statement, ParseError> {
    let t = inp.pop()?;
    rule!(t, {
      insert_stmt => InsertStmt::parse(t.into_inner()).map(|c| Statement::Insert(c))
      create_table => CreateTable::parse(t.into_inner()).map(|c| Statement::CreateTable(c))
      create_schema => CreateSchema::parse(t.into_inner()).map(|c| Statement::CreateSchema(c))
      create_function => CreateFunction::parse(t.into_inner()).map(|c| Statement::CreateFunction(c))
      transaction_stmt => TransactionStmt::parse(t.into_inner()).map(|c| Statement::Transaction(c))
      select_stmt => Select::parse(t.into_inner()).map(|c| Statement::Select(c))
      ret_stmt => ReturnStmt::parse(t.into_inner()).map(|c| Statement::Return(c))
      create_view => View::parse(t.into_inner()).map(|c| Statement::View(c))
    })
  }
}

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
