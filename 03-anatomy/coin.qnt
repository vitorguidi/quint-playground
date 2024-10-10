module coin {

    type Addr = str
    type UInt = int

    pure val MAX_UINT = 2^256 - 1
    pure def isUInt(i: int): bool = (0 <= i and i <= MAX_UINT)

    pure val ADDR = Set("null", "alice", "bob", "charlie", "eve")

    var minter: Addr
    var balances: Addr -> UInt

    val state = {minter: minter, balances: balances}
    val totalSupply = ADDR.fold(0, (sum, x) => sum + balances.get(x))

    action init: bool = {
        nondet sender = oneOf(ADDR)
        all {
            minter'= sender,
            balances' = ADDR.mapBy(a=>0)
        }

    }

    action mint(sender: Addr, receiver: Addr, amount: UInt) : bool = all  {
        sender == minter,
        isUInt(balances.get(receiver) + amount),
        isUInt(totalSupply + amount),
        val newBal = balances.get(receiver) + amount
        all {
            isUInt(newBal),
            balances' = balances.set(receiver, newBal),
            minter' = minter,
        }
    }

    action send(sender: Addr, receiver: Addr, amount: UInt) : bool = all {
        isUInt(balances.get(sender) - amount),
        isUInt(balances.get(receiver) + amount),
        isUInt(totalSupply + amount),

        if (receiver == sender) {
            balances' = balances
        } else {
            val newSenderBal = balances.get(sender) - amount
            val newReceiverBal = balances.get(receiver) + amount
            balances' = balances.set(receiver, newReceiverBal).set(sender, newSenderBal)
        },        
        minter' = minter,
    }

    action step : bool = {
        nondet sender = oneOf(ADDR)
        nondet receiver = oneOf(ADDR)
        nondet amount = 0.to(MAX_UINT).oneOf()
        any {
            mint(sender, receiver, amount),
            send(sender, receiver, amount),
        }
    }
    
    val balancesRangeInv: bool = 
        ADDR.forall(a => isUInt(balances.get(a)))

    val totalSupplyDoesNotOverflowInv: bool = {
        isUInt(totalSupply)
    }

    temporal NoSupplyOverflow: bool = always(totalSupplyDoesNotOverflowInv)


}