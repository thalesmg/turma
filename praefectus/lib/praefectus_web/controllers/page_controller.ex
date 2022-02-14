defmodule PraefectusWeb.PageController do
  use PraefectusWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
