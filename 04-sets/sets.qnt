module sets {
    
    type Pair = (str, str)
    pure def fst(pair: Pair): str = pair._1
    pure def snd(pair: Pair): str = pair._2
    pure val availablePairs = Set(
        ("BTC", "USDC"), ("BTC", "USDT"), ("ETH", "USDC"), ("ETH", "USDT"),
        ("EVMOS", "USDC"), ("EVMOS", "WETH"), ("ETH", "WETH"), ("ATOM", "EVMOS"),
        ("ATOM", "JUNO"), ("ATOM", "OSMO"), ("EVMOS", "JUNO"), ("ATOM", "JUNO"),
        ("EVMOS", "OSMO")
    )
    pure val availableUnorderedPairs =
        availablePairs.map(p => Set(p._1, p._2))
    pure def has_el(x: Set[Pair], y: Pair): bool = y.in(x)

  pure val atomPairs = availableUnorderedPairs.filter(p => p.contains("ATOM"))
  pure val quads =
    availableUnorderedPairs
      .powerset()
      .filter(pairs => pairs.size() == 4)
}