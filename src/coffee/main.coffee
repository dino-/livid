require ["shownav"], (nav) ->

   sizeContent = () ->
      contentHeight = window.innerHeight -
         ($ '#header').height() -
         ($ '#footer').height()

      ($ '#nav-container').height contentHeight


   ($ document).ready ->
      req = $.getJSON '/getShowList', (data) ->
         nav.createNavShows data
      req.error (resp) -> ($ 'body').html resp.responseText

      # Call this now and set it on window resize
      sizeContent()
      ($ window).resize sizeContent
