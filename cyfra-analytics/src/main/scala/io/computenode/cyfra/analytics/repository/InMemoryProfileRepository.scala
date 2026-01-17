package io.computenode.cyfra.analytics.repository

import cats.effect.{IO, Ref}
import io.computenode.cyfra.analytics.model.CustomerProfile

class InMemoryProfileRepository(profilesRef: Ref[IO, Map[Long, CustomerProfile]]) extends CustomerProfileRepository:

  def upsert(profile: CustomerProfile): IO[Unit] =
    profilesRef.update(_ + (profile.customerId -> profile))

  def upsertBatch(profiles: List[CustomerProfile]): IO[Unit] =
    profilesRef.update { current =>
      current ++ profiles.map(p => p.customerId -> p).toMap
    }

  def get(customerId: Long): IO[Option[CustomerProfile]] =
    profilesRef.get.map(_.get(customerId))

  def getByIds(customerIds: List[Long]): IO[List[CustomerProfile]] =
    profilesRef.get.map { profiles =>
      customerIds.flatMap(profiles.get)
    }

  def getAll: IO[List[CustomerProfile]] =
    profilesRef.get.map(_.values.toList)

  def count: IO[Int] =
    profilesRef.get.map(_.size)

object InMemoryProfileRepository:
  def create: IO[InMemoryProfileRepository] =
    Ref
      .of[IO, Map[Long, CustomerProfile]](Map.empty)
      .map(new InMemoryProfileRepository(_))
