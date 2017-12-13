use geom::Point;
use rand;

pub fn sparkles(n: usize) -> Vec<Point> { (0..n).map(|_| rand::random::<Point>()).collect() }