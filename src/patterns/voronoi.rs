use geom::Point;
use palette::Colora;
use shaders::Shader;
use std;
use textures::Texture;

pub fn voronoi(size: u32, sites: Vec<(Colora, Point)>) -> Texture {
    let shader = move |point: Point| {
        sites
            .iter()
            .min_by(|&&(_, p1), &&(_, p2)| if point.distance(p1) - point.distance(p2) > 0.0 {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Less
                    })
            .unwrap()
            .0
    };
    Texture::from_shader(size, &Shader::linear(shader))
}