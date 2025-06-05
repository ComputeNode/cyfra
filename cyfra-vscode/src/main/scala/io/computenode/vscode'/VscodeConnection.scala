package io.computenode.cyfra.vscode

import io.computenode.cyfra.vscode.VscodeConnection.Message

import java.net.http.{HttpClient, WebSocket}

class VscodeConnection(host: String, port: Int) {
  val ws = HttpClient
    .newHttpClient()
    .newWebSocketBuilder()
    .buildAsync(java.net.URI.create(s"ws://$host:$port"), new WebSocket.Listener {})
    .join()

  def send(message: Message): Unit =
    ws.sendText(message.toJson, true)
}

object VscodeConnection:
  trait Message:
    def toJson: String
  case class RenderingMessage(step: Int, totalSteps: Int, status: String) extends Message:
    def toJson: String = s"""{"type":"rendering","step":$step,"totalSteps":$totalSteps,"status":"$status"}"""
  case class RenderedMessage(path: String) extends Message:
    def toJson: String = s"""{"type":"rendered","path":"${escape(path)}"}"""
  case class VideoRenderedMessage(framesDir: String) extends Message:
    def toJson: String = s"""{"type":"videoRendered","framesDir":"${escape(framesDir)}"}"""

  private def escape(str: String): String =
    str.replace("\\", "\\\\").replace("\"", "\\\"")
