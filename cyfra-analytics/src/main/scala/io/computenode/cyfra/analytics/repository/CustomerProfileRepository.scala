package io.computenode.cyfra.analytics.repository

import cats.effect.IO
import io.computenode.cyfra.analytics.model.CustomerProfile

trait CustomerProfileRepository:
  
  def upsert(profile: CustomerProfile): IO[Unit]
  
  def upsertBatch(profiles: List[CustomerProfile]): IO[Unit]
  
  def get(customerId: Long): IO[Option[CustomerProfile]]
  
  def getByIds(customerIds: List[Long]): IO[List[CustomerProfile]]
  
  def getAll: IO[List[CustomerProfile]]
  
  def count: IO[Int]
