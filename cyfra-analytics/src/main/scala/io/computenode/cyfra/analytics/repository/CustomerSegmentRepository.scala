package io.computenode.cyfra.analytics.repository

import cats.effect.IO
import io.computenode.cyfra.analytics.model.{CustomerSegment, SegmentInfo}

trait CustomerSegmentRepository:
  
  def upsert(segment: CustomerSegment): IO[Unit]
  
  def upsertBatch(segments: List[CustomerSegment]): IO[Unit]
  
  def get(customerId: Long): IO[Option[CustomerSegment]]
  
  def getStats(profiles: List[(Long, Double)]): IO[List[SegmentInfo]]
