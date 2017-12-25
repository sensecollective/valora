use geom::Point;
use palette::Colora;
use std::rc::Rc;

#[derive(Clone)]
pub struct Colorer(Option<Rc<Fn(Point) -> Colora>>);

impl Default for Colorer {
    fn default() -> Self { Colorer(None) }
}

impl<F: 'static + Fn(Point) -> Colora> From<F> for Colorer {
    default fn from(f: F) -> Self { Colorer(Some(Rc::new(f))) }
}

impl<F: 'static + Fn(Point) -> Colora> From<Rc<F>> for Colorer {
    fn from(f: Rc<F>) -> Self { Colorer(Some(f.clone())) }
}

impl Colorer {
    pub fn color(&self, point: Point) -> Colora {
        match self.0 {
            None => Colora::rgb(0.0, 0.0, 0.0, 0.0),
            Some(ref f) => (f.as_ref())(point),
        }
    }
}