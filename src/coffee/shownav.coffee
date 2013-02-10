loadNavShows = () ->
   req = $.getJSON '/getShowList', (data) ->
      createNavShows data
      sizeContent()
      ($ window).resize sizeContent
      document.body.style.visibility = 'visible'
      ($ '#nav-shows li:first')[0].setSelection()
      $( document ).on  'keydown', navHandler
      $( document ).on  'click', clickHandler
   req.error (resp) -> ($ 'body').html resp.responseText


navHandler = (e)  ->
  console.log e.type
  sel = getSelection()
  console.log sel
  #sel.trigger e.type
  sel.handleEvent e

clickHandler = (e) ->
  console.log e.type


sizeContent = () ->
   contentHeight = window.innerHeight -
      ($ '#header').height() -
      ($ '#footer').height()
   ($ '#nav-container').height contentHeight


createNavShows = (alldata) ->
   ul = $ '#nav-shows'
   ul.empty()
   ul.append createShowLi showdata for showdata in alldata
   ($ '#nav-shows li' ).attr 'tabindex', (index, attr) -> index
   lidata = _.zip ($ '#nav-shows li'), alldata
   _.map lidata, setShowHandler


createShowLi = (showdata) ->
   "<li>#{showdata.title}</li>"


createNavEpisodes = (episodes) ->
   console.log "createNavEpisodes"
   console.log episodes
   ul = $ '#nav-episodes'
   ul.empty()
   ul.append createEpisodeLi episode for episode in episodes
   ($ '#nav-episodes li' ).attr 'tabindex', (index, attr) -> index
   lidata = _.zip ($ '#nav-episodes li'), episodes
   _.map lidata, setEpisodeHandler


createEpisodeLi = (epdata) ->
   "<li>#{epdata.title}</li>"


# Key handler for 'show' list navigation
#
# t is the tuple: (li, showdata)
setShowHandler = (t) ->
   #li = $ t[0]
   li = t[0]
   showdata = t[1]

   li.handleEvent = (e) ->
      key = e.keyCode
      target = $ this

      switch key
         when 38 # arrow up
            if target.prev()[0]
               target.prev()[0].setSelection()
         when 39 # arrow right 
            $( '#nav-episodes li:first' ).addClass 'ui-selected'
         when 40 # arrow down
            if target.next()[0]
               target.next()[0].setSelection()

   li.setSelection = () ->
      ($ '#nav-shows .ui-selected' ).removeClass 'ui-selected'
      createNavEpisodes showdata.episodes
      ($ this ).addClass 'ui-selected'
      


getSelection = () ->
   ep = ($ '#nav-episodes .ui-selected' )
   if ( ep.length )
      _.head ep
   else
      _.head ($ '#nav-shows .ui-selected' )


# Key handler for 'episode' list navigation
#
# t is the tuple: (li, epdata)
setEpisodeHandler = (t) ->
   #li = $ t[0]
   li = t[0]
   epdata = t[1]

   li.handleEvent = (e) ->
      key = e.keyCode
      target = $ this

      switch key
         when 13, 32 # enter or space
            playVideo epdata
         when 37 # arrow left
            ($ '#nav-episodes .ui-selected' )[0].removeSelection()
            ($ '#nav-shows .ui-selected' )[0].setSelection()
         when 38 # arrow up
            if ( target.is ($ '#nav-episodes li:first') )
               ($ '#nav-episodes li:last')[0].setSelection()
            else target.prev()[0].setSelection()

            target[0].removeSelection()
         when 40 # arrow down
            if ( target.is ($ '#nav-episodes li:last') )
               ($ '#nav-episodes li:first')[0].setSelection()
            else target.next()[0].setSelection()

            target[0].removeSelection()
         when 46, 68 # delete or'd' key
            showDeleteDialog this, epdata

   li.setSelection = () ->
      ($ this ).addClass 'ui-selected'

   li.removeSelection = () ->
      ($ this ).removeClass 'ui-selected'


playVideo = (ep) ->
   $.ajax {
      url: 'playVideo'
      type: 'POST'
      data: ep.playpath
      dataType: 'text'
      contentType: 'text/plain'
      #TODO: success: () -> console.log 'success'
   }


showDeleteDialog = (li, ep) ->
   ($ '#del-confirm-dialog' ).dialog {
      width: 400
      modal: true
      close: () -> ($ li).focus()
      buttons:
         Delete: () ->
            delEpisode ep
            ($ this).dialog 'close'

         Cancel: () ->
            ($ this).dialog 'close'
   }
   ($ '#del-confirm-dialog' ).dialog 'open'


delEpisode = (ep) ->
   $.ajax {
      url: 'delVideo'
      type: 'POST'
      data: ep.playpath
      dataType: 'text'
      contentType: 'text/plain'
      success: () -> loadNavShows()
   }


define
   loadNavShows: loadNavShows
