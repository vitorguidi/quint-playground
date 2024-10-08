module booleans {
    pure val myFalse = false
    pure val myTrue = true

    pure def myNot(x) = not(x)
    pure def myNot2(x) = not(x)

    pure def myAnd(x, y, z) = and(x,y,z)
    pure def myAnd2(x, y, z) = x.and(y).and(z)
    pure def myAnd3(x,y,z) = and{x, y, z}

    pure def myImplication(x, y) = x implies y
    pure def myIff(x, y) = x iff y
    pure def myIff2(x,y) = iff(x, y)

}