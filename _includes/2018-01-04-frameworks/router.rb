Action = Struct.new(:controller, :action, :params)

Route = Struct.new(:check_requst, :check_action,
                   :requst_to_action, :action_to_path) do

  def self.all(routes)
    routes.reduce(:or)
  end

  def or(other)
    Route.new(
      ->(x) { check_request(x) || other.check_request(x) },
      ->(x) { check_action(x) || other.check_action(x) },
      ->(x) { check_request(x) ? reqest_to_action(x) : other.reqest_to_action(x) },
      ->(x) { check_action(x) ? action_to_path(x) : other.action_to_path(x) }
    )
  end
end

def uri_match(method, uri, controller, action, params = {})
  params = Params.new(params) # implements === to match request params
  Route.new(
    ->(x) { x.method == method && pattern === x.uri && params === x.params },
    ->(x) { x.controller == controller && x.action == action },
    ->(x) { Action.new(controller, action, x.params) },
    ->(x) { build_path(uri, x.params) }
  )
end

Route.all [
  uri_match('GET', '/users', UsersController, :index),
  uri_match('GET', %r{\Ausers/\d+\z}, UsersController, :show),
  uri_match('POST', '/users', UsersController, :create),
  uri_match('PUT', %r{\Ausers/\d+\z}, UsersController, :update),
  uri_match('DELETE', %r{\Ausers/\d+\z}, UsersController, :destroy)
]
