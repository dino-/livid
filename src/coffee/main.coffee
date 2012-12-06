require ["shownav"], (nav) ->
   ($ document).ready ->
      req = $.getJSON '/getShowList', (data) ->
         nav.createNavShows data
      req.error (resp) -> ($ 'body').html resp.responseText

