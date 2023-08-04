PATTERN = """        bgroup
          "{methodName} with {monadLayer}/{pureLayer}"
          [ bench "without dest" . {methodLineCont}
              let res = parseWithoutDest sampleData
              resInRegion <- compact res
              {monadLayer} . {pureLayer} . getCompact $ resInRegion
            ,
            bench "with dest" . {methodLineCont}
              {monadLayer} . {pureLayer} . parseWithDest $ sampleData
          ],"""



methods = [
    ("whnfIO", "whnfIO $ do"),
    ("nfIO", "nfIO $ do"),
    ("whnfAppIO", "(flip whnfAppIO) sampleData $ \sampleData -> do"),
    ("nfAppIO", "(flip nfAppIO) sampleData $ \sampleData -> do"),
]

monadLayers = [
    "return",
    "evaluate",
]

pureLayers = [
    "id",
    "force",
    "rnf",
]

for (methodName, methodLineCont) in methods:
    for monadLayer in monadLayers:
        for pureLayer in pureLayers:
            print(PATTERN.format(
                methodName=methodName,
                methodLineCont=methodLineCont,
                monadLayer=monadLayer,
                pureLayer=pureLayer
            ))
