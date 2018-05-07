require ["shownav"], (nav) ->

   ($ document).ready ->
      req = $.get '/getVersion', (data) ->
         ($ '#version').append "<span>#{data}</span>"
      #req.error (resp) -> ($ 'body').html resp.responseText

      nav.loadNavShows()
