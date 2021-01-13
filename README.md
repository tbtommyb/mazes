# mazes

A Clojure library designed to create mazes, based on [Mazes for Programmers](http://www.mazesforprogrammers.com/).

## Usage

```
mazes.core> (pr/out (pr/ascii-grid (algo/binary-tree (gr/new-grid 10 10))))
+---+---+---+---+---+---+---+---+---+---+
|                                       |
+   +---+---+   +   +   +   +---+   +   +
|   |           |   |   |   |       |   |
+---+---+---+---+---+   +---+   +   +   +
|                       |       |   |   |
+---+   +   +   +---+---+   +---+   +   +
|       |   |   |           |       |   |
+   +   +---+   +---+   +---+   +   +   +
|   |   |       |       |       |   |   |
+   +---+   +   +   +   +   +   +---+   +
|   |       |   |   |   |   |   |       |
+---+---+   +   +   +   +   +---+---+   +
|           |   |   |   |   |           |
+   +   +   +---+---+   +   +---+   +   +
|   |   |   |           |   |       |   |
+   +---+   +   +---+---+   +   +   +   +
|   |       |   |           |   |   |   |
+   +   +---+---+---+---+---+   +   +   +
|   |   |                       |   |   |
+---+---+---+---+---+---+---+---+---+---+

mazes.core> (pr/out (pr/ascii-grid (algo/sidewinder (gr/new-grid 10 10))))
+---+---+---+---+---+---+---+---+---+---+
|                                       |
+---+   +---+   +   +---+   +---+   +---+
|       |       |   |           |       |
+---+   +---+---+---+---+---+   +---+---+
|           |                           |
+---+   +---+---+---+---+---+   +   +   +
|                           |   |   |   |
+   +---+   +---+   +---+---+---+---+   +
|       |   |                       |   |
+---+   +   +   +---+---+   +---+   +---+
|       |   |   |           |           |
+   +---+---+   +   +   +---+   +   +---+
|           |   |   |       |   |       |
+   +---+   +---+   +   +   +---+---+   +
|       |       |   |   |           |   |
+   +---+   +   +   +   +---+   +---+   +
|       |   |   |   |       |   |       |
+   +---+   +   +   +---+---+   +   +---+
|       |   |   |           |   |       |
+---+---+---+---+---+---+---+---+---+---+

mazes.core> (def maze (algo/sidewinder (gr/new-grid 8 8)))
mazes.core> (pr/out (pr/ascii-grid maze {:distances (dist/longest-path maze)}))
+---+---+---+---+---+---+---+---+
| f   g   h   i   j   k   l     |
+   +---+---+   +   +---+   +---+
| e   d   c |   |   |     m     |
+   +   +   +---+---+---+   +---+
|   |   | b   a   9 |     n     |
+---+   +---+---+   +   +   +   +
|       |     7   8 |   | o |   |
+   +   +   +   +   +---+   +   +
|   |   |   | 6 |       | p |   |
+   +---+   +   +---+---+   +   +
|   |       | 5     | r   q |   |
+---+   +---+   +---+   +   +---+
|       | 3   4     | s |       |
+   +   +   +---+---+   +---+   +
|   |   | 2   1   0 | t   u |   |
+---+---+---+---+---+---+---+---+

mazes.core> (def maze (algo/sidewinder (gr/new-grid 12 12)))
mazes.core> (def dist (dist/dijkstra maze [0 0]))
mazes.core> (pr/png-out maze "images/sidewinder.png" {:distances dist})
```

![Sidewinder coloured](https://github.com/tbtommyb/mazes/blob/master/images/readme-sidewinder.png?raw=true)

```
mazes.core> (def maze (algo/recursive-backtracker (polar/new-grid 20)))
mazes.core> (def distances (dist/dijkstra maze [5 18]))
mazes.core> (pr/png-out maze "polar-maze-distances.png" {:distances distances})
```

![Polar coloured](https://github.com/tbtommyb/mazes/blob/master/images/readme-polar-maze-distances.png?raw=true)

## License

Copyright © 2020 Tom Johnson

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
