loadNavShows = () ->
   req = $.getJSON '/getShowList', (data) ->
      createNavShows data
      sizeContent()
      ($ window).resize sizeContent
      document.body.style.visibility = 'visible'
      ($ '#nav-shows li:first').focus()
   req.error (resp) -> ($ 'body').html resp.responseText


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
   li = $ t[0]
   showdata = t[1]

   li.bind {

      keydown: (e) ->
         key = e.keyCode
         target = $ e.currentTarget

         switch key
            when 38 # arrow up
               target.prev().focus()
            when 39 # arrow right 
               $( '#nav-episodes li:first' ).focus()
            when 40 # arrow down
               target.next().focus()

      focusin: (e) ->
         ($ '#nav-shows .ui-selected' ).removeClass 'ui-selected'
         createNavEpisodes showdata.episodes
         ($ e.currentTarget).addClass 'ui-selected'

   }


# Key handler for 'episode' list navigation
#
# t is the tuple: (li, epdata)
setEpisodeHandler = (t) ->
   li = $ t[0]
   epdata = t[1]

   li.bind {

      keydown: (e) ->
         key = e.keyCode
         target = $ e.currentTarget

         switch key
            when 13, 32 # enter or space
               playVideo epdata
            when 37 # arrow left
               ($ '#nav-shows .ui-selected' ).focus()
            when 38 # arrow up
               if ( target.is ($ '#nav-episodes li:first') )
                  ($ '#nav-episodes li:last').focus()
               else target.prev().focus()
            when 40 # arrow down
               if ( target.is ($ '#nav-episodes li:last') )
                  ($ '#nav-episodes li:first').focus()
               else target.next().focus()
            when 46, 68 # delete or'd' key
               showDeleteDialog this, epdata

      focusin: (e) ->
         ($ e.currentTarget).addClass 'ui-selected'

      focusout: (e) ->
         ($ e.currentTarget).removeClass 'ui-selected'

   }


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
