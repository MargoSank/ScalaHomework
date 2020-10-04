
// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.


object ClassesAndTraits {

  sealed trait Shape2D[A] extends Locate2D with Bounded2D with Properties2D[A]

  sealed trait Locate2D {
    def x: Double
    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }


  // Add method `area` to 2D shapes.
  sealed trait Properties2D[A]{
    def area: Double
    def move(dx: Double, dy: Double): A
  }

  object Origin2D extends Locate2D {
    override def x: Double = 0
    override def y: Double = 0
  }

  final case class Point(x: Double, y: Double) extends Shape2D[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
    override def area: Double = 0
    def distanceTo(point2: Point) = Math.sqrt(Math.pow((point2.x - x),2) + Math.pow((point2.y - y),2))
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D[Circle] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)
    override def area: Double = (Math.PI * Math.pow(radius,2)) / radius
  }

  case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape2D[Rectangle] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - width / 2
    override def maxX: Double = centerX + width / 2
    override def minY: Double = centerY - height / 2
    override def maxY: Double = centerY + height / 2
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(centerX + dx, centerY + dy, width, height)
    override def area: Double = width * height
  }

  // Add additional 2D shapes such as triangle and square.
  final case class Triangle(vertex: Set[Point]) extends Shape2D[Triangle] {
    def sideLenght(a: Point, b: Point) = Math.sqrt(Math.pow((b.x - a.x),2) + Math.pow((b.y - a.y),2))
    val points: List[Point] = vertex.toList
    def sideA: Double = points(0).distanceTo(points(1))
    def sideB: Double = points(0).distanceTo(points(2))
    def sideC: Double = points(1).distanceTo(points(2))
    def p: Double = ( sideA + sideB + sideC ) / 2
    override def move(dx: Double, dy: Double): Triangle = Triangle(vertex.map(_.move(dx, dy)))
    override def area: Double = Math.sqrt(p * (p - sideA) * (p - sideB) * (p - sideC))

    override def x: Double = vertex.head.x
    override def y: Double = vertex.head.y
    override def minX: Double = vertex.map(_.x).min
    override def maxX: Double = vertex.map(_.x).max
    override def minY: Double = vertex.map(_.y).min
    override def maxY: Double = vertex.map(_.y).max
  }

  final case class Square(override val centerX: Double, override val centerY: Double, override val width: Double,
                          override val height: Double) extends Rectangle(centerX, centerY, width, height) {
    override def move(dx: Double, dy: Double): Square = Square(centerX + dx, centerY + dy, width, height)
  }


  // 3D shapes classes
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  sealed trait Shape3D[A] extends  Properties3D[A]

  sealed trait Properties3D[A]{
    def surfaceArea: Double
    def volume: Double
    def move(dx: Double, dy: Double, dz: Double): A
  }

  // origin, point, sphere, cube, cuboid, 3D triangle
  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D[Point3D] {
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  final case class Sphere(center: Point3D, radius: Double) extends Shape3D[Sphere] {
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(center.move(dx, dy, dz), radius)
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = (3/4) * Math.PI * Math.pow(radius, 3)
  }

  case class Cuboid(cornerA: Point3D, cornerB: Point3D) extends Shape3D[Cuboid] {
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(cornerA.move(dx, dy, dz), cornerB.move(dx, dy, dz))
    def width():Double = Math.abs(cornerB.x - cornerA.x)
    def height():Double = Math.abs(cornerB.y - cornerA.y)
    def depth():Double = Math.abs(cornerB.z - cornerA.z)
    override def surfaceArea: Double = 2 * (width * height + height * depth + width * height)
    override def volume: Double = width * height * depth
  }

  final case class Cube(cornerA: Point3D, cornerB: Point3D ) extends Shape3D[Cube] {
    private val cuboid: Cuboid = Cuboid(cornerA, cornerB)
    override def move(dx: Double, dy: Double, dz: Double):Cube = Cube(cornerA.move(dx, dx, dz), cornerB.move(dx, dy, dz))
    override def surfaceArea: Double = cuboid.surfaceArea
    override def volume: Double = cuboid.volume
  }

  def main(args: Array[String]): Unit = {

  }
}