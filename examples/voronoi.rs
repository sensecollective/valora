
extern crate rand;
extern crate valora;

use rand::Rng;
use valora::errors::*;
use valora::geom::*;
use valora::palette::*;
use valora::patterns::sparkles::*;
use valora::patterns::voronoi::*;
use valora::render::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Voronoi {}

impl Sketch for Voronoi {
    fn draw(&self, ctx: &SketchContext) -> Result<Render> {
        let sparkles = sparkles(10);
        let debug_dots =
            (Shader::constant(Colora::hsv(RgbHue::from(0.0), 0.0, 1.0, 1.0)),
             sparkles
                 .clone()
                 .into_iter()
                 .map(|p| Geometry::Ellipse(ellipse::Ellipse::circle(p, 0.005, 0.0)))
                 .collect());
        Render::new()
            .add_texture(voronoi(ctx.cfg.size,
                                 sparkles
                                     .clone()
                                     .into_iter()
                                     .map(|p| {
                (Colora::hsv(RgbHue::from(220.0 +
                                          rand::OsRng::new().unwrap().gen_range(0.0, 1.0) * 50.0),
                             1.0,
                             1.0,
                             1.0),
                 p)
            })
                                     .collect()))
            .add(debug_dots)
    }
}

fn main() {
    sketch(SketchCfg { size: 700, root_frame_filename: None }, Voronoi {}).expect("working sketch");
}