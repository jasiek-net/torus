# Torus

###Description
This is Smalltalk implementation of Torus - multidimensional linked list.

###Content
To use package you have to install Torus.pac by Dolphin package browser. In file Tests.st there are some tests for Torus, including infinite list. The whole project was created for collage, writing in Smalltalk is a modest entertainment.

###Command
```Torus shape: #(4, 6, 5)``` - return first point (0,0,0) of torus, with dimensions sizes defined by ```shape```

```point value``` - get value of point

```point value: 5``` - set value of point

```point + 3``` - return next point after 3rd coordinate (e.g. (4,2,1) -> (4,2,2))

```point - 2``` - return previous point after 2nd coordinate (e.g. (4,5,3) -> (4,4,3))

```point @ #(2, 1, 3)``` - return point distant by the specified vector (e.g. (0,0,0) -> (2,1,3))

```point | 2``` - return list of all next point after 2nd coordinate

```point % (2 -> 3)``` - return list size 2 after 3rd coordinate

```list do: block``` - visited all points from list and execute ```block``` on points

```s , t``` - concatenate two lists of points


```list | 3``` - return concatenation of all lists returned by execution ```| 3``` on points of the list

```list % (2 -> 3)``` - analogously to a command above

```point & block``` - return list of points created in this way: we execute ```block``` on ```point```, if it's return nil we reach end of list, otherwise returned value is a further part of the list (it could create infinite list)
