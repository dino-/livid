($ document).ready ->

   $(".ui-widget-content").bind {
      keydown: (e) ->
         key = e.keyCode
         target = $ e.currentTarget
    
         switch key
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
   ($ "li").first().focus()

