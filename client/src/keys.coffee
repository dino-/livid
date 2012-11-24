($ document).ready ->
   createNavShows data
###
   # show list navigation 
   $("#nav-shows li").bind {
      keydown: (e) ->
         key = e.keyCode
         target = $ e.currentTarget
    
         switch key
            when 38 # arrow up
               target.prev().focus()
               break
            when 39 # arrow right 
               # TODO: update episodes
               break
            when 40 # arrow down
               target.next().focus()
               break
    
      focusin: (e) ->
         ($ e.currentTarget).addClass "ui-selected"

    
      focusout: (e) ->
         ($ e.currentTarget).removeClass "ui-selected"

   }
   ($ "li").first().focus()

  
   # episodes list navigation
   $("#nav-episodes li").bind {
      keydown: (e) ->
         key = e.keyCode
         target = $ e.currentTarget
    
         switch key
            when 37 # arrow left
               # TODO - deselect
               break
            when 38 # arrow up
               target.prev().focus()
               break
            when 40 # arrow down
               target.next().focus()
               break
    
      focusin: (e) ->
         ($ e.currentTarget).addClass "ui-selected"

    
      focusout: (e) ->
         ($ e.currentTarget).removeClass "ui-selected"

   }
###
 

createNavShows = (alldata) ->
   ul = $ '#nav-shows'
   ul.append createShowLi showdata for showdata in alldata
   $( "#nav-shows li" ).attr 'tabindex', (index, attr) -> index
   $( "#nav-shows li").bind showHandler


createShowLi = (showdata) ->
   "<li>#{showdata.title}</li>"


createNavEpisodes = (episodes) ->
   #$( '#nav-episodes li' ).remove()
   ul = $ '#nav-episodes'
   ul.empty()
   ul.append createEpisodeLi epdata for epdata in episodes

 
createEpisodeLi = (epdata) ->
   "<li>#{epdata.title}</li>"


showHandler =  {
   keydown: (e) ->
      key = e.keyCode
      target = $ e.currentTarget
    
      switch key
         when 38 # arrow up
            target.prev().focus()
            break
         when 39 # arrow right 
            # TODO: update episodes
            break
         when 40 # arrow down
            target.next().focus()
            break
    
   focusin: (e) ->
      ($ e.currentTarget).addClass "ui-selected"
      showdata =  getShowData ($ e.currentTarget).attr( 'tabindex' )
      console.log "showdata=" + JSON.stringify showdata
      createNavEpisodes showdata.episodes

    
   focusout: (e) ->
      ($ e.currentTarget).removeClass "ui-selected"

}


getShowData = (index) ->
   data[index]
