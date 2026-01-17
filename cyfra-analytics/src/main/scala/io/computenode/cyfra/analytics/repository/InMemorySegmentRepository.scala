package io.computenode.cyfra.analytics.repository

import cats.effect.{IO, Ref}
import io.computenode.cyfra.analytics.model.{CustomerSegment, SegmentInfo}

class InMemorySegmentRepository(segmentsRef: Ref[IO, Map[Long, CustomerSegment]]) extends CustomerSegmentRepository:

  def upsert(segment: CustomerSegment): IO[Unit] =
    segmentsRef.update(_ + (segment.customerId -> segment))

  def upsertBatch(segments: List[CustomerSegment]): IO[Unit] =
    segmentsRef.update { current =>
      current ++ segments.map(s => s.customerId -> s).toMap
    }

  def get(customerId: Long): IO[Option[CustomerSegment]] =
    segmentsRef.get.map(_.get(customerId))

  def getStats(profiles: List[(Long, Double)]): IO[List[SegmentInfo]] =
    segmentsRef.get.map { segments =>
      val profileMap = profiles.toMap
      segments.values
        .groupBy(_.segmentName)
        .map { case (name, segs) =>
          val customerIds = segs.map(_.customerId).toSet
          val relevantSpend = customerIds.flatMap(profileMap.get)
          SegmentInfo(
            name = name,
            customerCount = segs.size,
            avgLifetimeValue = if relevantSpend.nonEmpty then relevantSpend.sum / relevantSpend.size else 0.0,
            avgRecency = 0.0,
          )
        }
        .toList
        .sortBy(-_.customerCount)
    }

object InMemorySegmentRepository:
  def create: IO[InMemorySegmentRepository] =
    Ref
      .of[IO, Map[Long, CustomerSegment]](Map.empty)
      .map(new InMemorySegmentRepository(_))
