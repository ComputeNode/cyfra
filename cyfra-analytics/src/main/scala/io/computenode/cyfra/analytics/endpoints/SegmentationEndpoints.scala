package io.computenode.cyfra.analytics.endpoints

import cats.effect.IO
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.capabilities.fs2.Fs2Streams
import io.circe.generic.auto.*
import io.computenode.cyfra.analytics.model.*
import io.computenode.cyfra.analytics.service.SegmentationService
import io.computenode.cyfra.analytics.repository.CentroidsRepository
import io.computenode.cyfra.fs2interop.GCluster.FCMCentroids

class SegmentationEndpoints(
  service: SegmentationService,
  centroidsRepo: CentroidsRepository
):

  private val baseEndpoint = endpoint
    .in("api" / "v1")
    .errorOut(jsonBody[ErrorResponse])

  val submitTransaction: PublicEndpoint[Transaction, ErrorResponse, Unit, Any] =
    baseEndpoint.post
      .in("transactions")
      .in(jsonBody[Transaction])
      .out(statusCode(sttp.model.StatusCode.Accepted))
      .description("Submit a transaction for processing")

  val submitBatchTransactions: PublicEndpoint[List[Transaction], ErrorResponse, BatchResponse, Any] =
    baseEndpoint.post
      .in("transactions" / "batch")
      .in(jsonBody[List[Transaction]])
      .out(jsonBody[BatchResponse])
      .description("Submit a batch of transactions for bulk processing")

  val getCustomer: PublicEndpoint[Long, ErrorResponse, CustomerResponse, Any] =
    baseEndpoint.get
      .in("customers" / path[Long]("customerId"))
      .out(jsonBody[CustomerResponse])
      .description("Get customer segment and profile")

  val getSegments: PublicEndpoint[Unit, ErrorResponse, SegmentsResponse, Any] =
    baseEndpoint.get
      .in("segments")
      .out(jsonBody[SegmentsResponse])
      .description("List all customer segments")

  val getCentroids: PublicEndpoint[Unit, ErrorResponse, CentroidsResponse, Any] =
    baseEndpoint.get
      .in("centroids")
      .out(jsonBody[CentroidsResponse])
      .description("Get current segment centroids")

  val updateCentroids: PublicEndpoint[CentroidsUpdate, ErrorResponse, CentroidsResponse, Any] =
    baseEndpoint.put
      .in("centroids")
      .in(jsonBody[CentroidsUpdate])
      .out(jsonBody[CentroidsResponse])
      .description("Update segment centroids")

  val getHealth: PublicEndpoint[Unit, ErrorResponse, HealthStatus, Any] =
    endpoint.get
      .in("health")
      .out(jsonBody[HealthStatus])
      .errorOut(jsonBody[ErrorResponse])
      .description("Service health check")

  def serverEndpoints: List[ServerEndpoint[Fs2Streams[IO], IO]] = List(
    submitTransaction.serverLogic(tx => service.submitTransaction(tx).map(_ => Right(()))),
    
    submitBatchTransactions.serverLogic { transactions =>
      service.submitBatch(transactions).map { _ =>
        Right(BatchResponse(
          accepted = transactions.size,
          message = s"Accepted ${transactions.size} transactions for processing"
        ))
      }
    },
    
    getCustomer.serverLogic(customerId =>
      service.getCustomerInfo(customerId).map {
        case Some(response) => Right(response)
        case None => Left(ErrorResponse(s"Customer $customerId not found"))
      }
    ),
    
    getSegments.serverLogic(_ => service.getSegments.map(Right(_))),
    
    getCentroids.serverLogic(_ =>
      centroidsRepo.get.map { centroids =>
        Right(CentroidsResponse(
          labels = centroids.labels,
          numClusters = centroids.labels.size,
          numFeatures = centroids.values.length / centroids.labels.size
        ))
      }
    ),
    
    updateCentroids.serverLogic { update =>
      val centroids = FCMCentroids(update.labels, update.values)
      centroidsRepo.update(centroids).map { _ =>
        Right(CentroidsResponse(
          labels = centroids.labels,
          numClusters = centroids.labels.size,
          numFeatures = centroids.values.length / centroids.labels.size
        ))
      }
    },
    
    getHealth.serverLogic(_ => service.getHealth.map(Right(_)))
  )

  val all = List(submitTransaction, submitBatchTransactions, getCustomer, getSegments, getCentroids, updateCentroids, getHealth)
