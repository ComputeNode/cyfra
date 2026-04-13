package io.computenode.cyfra.utility.cats

type ~>[F[_], G[_]] = FunctionK[F, G]
