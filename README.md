# Monte Carlo Integration to Calculate Pi
[Monte Carlo Integration](https://en.wikipedia.org/wiki/Monte_Carlo_integration) is a technique that uses random sampling to approximate a value. By combining this technique with equations for a circle, we can approximate Pi.

### Given:
1. the Unit Circle (a radius of 1 and centered at <0, 0>)
2. the equation for the area of a circle (`Ac = Pi * r^2`)
3. the equation for the area of a rectangle (`Ar = width * height`)
4. the simple equation for a circle (`x^2 + y^2 = r^2`)

### Combined:
1. the area of the Unit Circle is Pi, because `r = 1`
2. the area of a square around the unit circle is 4, because `width = 2, height = 2`
3. points inside the circle satisfy the equation `x^2 + y^2 <= 1`, because `r = 1`

### Solving:
 Generate a uniform distribution of points in x = [-1, 1], y = [-1, 1]. The ratio of points in the circle to the square is approximately the ratio of their areas (Pi / 4). Since every point will be within the square, Pi ~= 4 * (num_in_circle / total_samples)
