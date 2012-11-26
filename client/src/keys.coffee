($ document).ready ->
   createNavShows data


createNavShows = (alldata) ->
   ul = $ '#nav-shows'
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
               break
            when 39 # arrow right 
               $( '#nav-episodes li:first' ).focus()
               break
            when 40 # arrow down
               target.next().focus()
               break
       
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
            when 37 # arrow left
               ($ '#nav-shows .ui-selected' ).focus()
               break
            when 38 # arrow up
               target.prev().focus()
               break
            when 40 # arrow down
               target.next().focus()
               break
       
      focusin: (e) ->
         ($ e.currentTarget).addClass 'ui-selected'

      focusout: (e) ->
         ($ e.currentTarget).removeClass 'ui-selected'

   }
