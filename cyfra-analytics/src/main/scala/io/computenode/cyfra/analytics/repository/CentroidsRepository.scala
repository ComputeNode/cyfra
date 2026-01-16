package io.computenode.cyfra.analytics.repository

import cats.effect.IO
import io.computenode.cyfra.fs2interop.GCluster.FCMCentroids

trait CentroidsRepository:
  
  def get: IO[FCMCentroids]
  
  def update(centroids: FCMCentroids): IO[Unit]
  
  def getLabels: IO[Vector[String]]
