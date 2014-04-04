import CLPFD

main = domain [x,y] (-10) 10 & y =# x *# x & labeling [x,y] where x,y free
