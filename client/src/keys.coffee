($ document).ready ->

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
 
