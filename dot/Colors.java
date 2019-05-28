// http://www.umlgraph.org/doc/indexw.html

/**
 * @opt edgecolor "yellow"
 * @opt nodefontname "Times"
 * @opt bgcolor ".7 .9 1"
 * @opt nodefillcolor "#a0a0a0"
 * @opt nodefontsize 14
 * @hidden
 */
class UMLOptions{}

/**
 * @opt nodefontname "Helvetica-Bold"
 * @opt nodefontcolor "white"
 * @composed - - - Red
 * @composed - - - Green
 * @composed - - - Blue
 * @opt attributes
 * @opt visibility
 * @opt types
 */
class Pixel {
        private int x, y;
        public void setColor(ColorValue v) {}
}

/** @opt nodefillcolor red */
class Red {}

/** @opt nodefillcolor green */
class Green {}

/** @opt nodefillcolor blue */
class Blue {}

/** @hidden */
class ColorValue{}
