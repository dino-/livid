createNavShows = (alldata) ->
   ul = $ '#nav-shows'
   ul.append createShowLi showdata for showdata in alldata
   ($ '#nav-shows li' ).attr 'tabindex', (index, attr) -> index
   lidata = _.zip ($ '#nav-shows li'), alldata
   _.map lidata, setShowHandler
   ($ '#nav-shows li:first').focus()


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
               playEpisode epdata.title
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
               delEpisode epdata.title

      focusin: (e) ->
         ($ e.currentTarget).addClass 'ui-selected'

      focusout: (e) ->
         ($ e.currentTarget).removeClass 'ui-selected'

   }


playEpisode = (ep) ->
   alert 'play: ' + ep


delEpisode = (ep) ->
   alert 'delete: ' + ep


define
   createNavShows: createNavShows
