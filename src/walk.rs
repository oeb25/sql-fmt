trait Walk<T> {
  fn walk<F, S>(&self, f: F)
  where
    F: Fn(T) -> S;
}

trait Transform<T: Transform<S>, S = ()>: Walk<T>
where
  S: Transform<()>,
{
  fn transform(&self) {
    self.walk(|x| x.transform());
  }
}

impl Walk<()> for () {
  fn walk<F, S>(&self, f: F)
  where
    F: Fn(()) -> S,
  {
    // noop
  }
}

impl Transform<()> for () {}

enum Brand {
  Ford,
}

impl Walk<()> for Brand {
  fn walk<F, S>(&self, f: F)
  where
    F: Fn(()) -> S,
  {
    // noop
  }
}

struct Car {
  brand: Brand,
}

impl Walk<Brand> for Car {
  fn walk<F, S>(&self, f: F)
  where
    F: Fn(Brand) -> S,
  {
    self.brand.walk(f)
  }
}

impl Transform<Car> for Car {}
