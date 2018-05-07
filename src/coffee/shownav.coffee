loadNavShows = ->
   req = $.getJSON '/getShowList', (data) ->
      createNavShows data
      #sizeContent()
      ($ window).resize sizeContent
      document.body.style.visibility = 'visible'

      ($ document).keydown ( (event) -> switch event.which
         when 37  # left arrow
            sel = $ '#nav-shows ul li[class~=selected]'
            sel.click()
            scrollIntoViewIfNeeded sel
            #($ '#nav-episodes ul').get(0).scrollTop()
            ($ '#nav-episodes ul').scrollTop()
         when 39  # right arrow
            sel = $ '#nav-episodes ul li:first-child'
            sel.click()
            scrollIntoViewIfNeeded sel
         when 38  # up arrow
            newSel = getSelection().prev()
            newSel.click()
            scrollIntoViewIfNeeded newSel
         when 40  # down arrow
            newSel = getSelection().next()
            newSel.click()
            scrollIntoViewIfNeeded newSel
         when 13, 32  # enter or space
            playpath = ($ '#nav-episodes ul li[class~=selected]').data 'playpath'
            if playpath then playVideo playpath
         when 27  # escape
            ($ '[class~=mdl-layout__drawer-button]').click()
         else
            console.log "Unknown key event: #{event.which}"
      )

      # Select the first show
      ($ '#nav-shows ul li:first-child').click()

   #req.error (resp) -> ($ 'body').html resp.responseText


scrollIntoViewIfNeeded = (target) ->
   if target.length == 0 then return
   domObj = target.get 0

   console.log target.parent().get(0).getBoundingClientRect()
   r = domObj.getBoundingClientRect()
   console.log r

   rect = domObj.getBoundingClientRect()
   if rect.bottom > window.innerHeight
      domObj.scrollIntoView false
   if rect.top < 0
      domObj.scrollIntoView true

#scrollIntoViewIfNeeded = (target) ->
#   if target.length == 0 then return
#   domObj = target.get 0
#   rect = domObj.getBoundingClientRect()
#   if rect.bottom > window.innerHeight
#      domObj.scrollIntoView false
#   if rect.top < 0
#      domObj.scrollIntoView true


# FIXME This code doesn't even run, ugh
#scrollToSimple = (target) ->
#    container = this.first()  # Only scrolls the first matched container
#
#    pos = target.position
#    height = target.outerHeight
#    containerScrollTop = container.scrollTop
#    containerHeight = container.height
#    top = pos.top + containerScrollTop  # position.top is relative to the scrollTop of the containing element
#
#    paddingPx = containerHeight * 0.15  # padding keeps the target from being butted up against the top / bottom of the container after scroll
#
#   if top < containerScrollTop  # scroll up
#      container.scrollTop(top - paddingPx)
#   else if (top + height > containerScrollTop + containerHeight)  # scroll down
#      container.scrollTop(top + height - containerHeight + paddingPx)


getSelection = ->
   selected = ($ '#nav-episodes ul li[class~=selected]')
   if selected.length == 0
      ($ '#nav-shows ul li[class~=selected]')
   else
      selected


setDate = (contents) ->
   d = ($ '#date')
   d.empty()
   d.append contents


clearDate = -> setDate ""


select = (li) ->
   # Isolate the (parent) list box
   lb = li.parent()

   # Clear all selected things there
   (lb.find 'li').removeClass 'selected'
   
   li.addClass 'selected'


sizeContent = ->
   contentHeight = window.innerHeight -
      ($ '#header').height() -
      ($ '#footer').height() - 20
   ($ '#nav-container').height contentHeight


createNavShows = (alldata) ->
   lb = $ "<ul class='mdl-list'></ul>"

   lb.append ( alldata.map (show) ->
      li = $ "<li class='mdl-list__item'><span class='mdl-list__item-primary-content'>#{show.title}</span></li>"
      li.click ->
         select li
         createNavEpisodes show.episodes
         clearDate()
      return li
   )

   showsDiv = $ '#nav-shows'
   showsDiv.empty()
   showsDiv.append lb

# FIXME
#createNavShows = (alldata) ->
#   lb = $ "<ul class='mdl-list'></ul>"
#
#   alldata.map (show) ->
#      #($ "<li class='mdl-list__item'><span class='mdl-list__item-primary-content'>#{show.title}</span></li>").appendTo lb
#      lb.append ($ "<li class='mdl-list__item'><span class='mdl-list__item-primary-content'>#{show.title}</span></li>")
#
#   showsdiv = $ '#nav-shows'
#   showsdiv.empty()
#   showsdiv.append lb

#class ShowItem extends Control
#   title: (showdata) -> this.content showdata.title
#   episodes: Control.property()
#
#
#createNavShows = (alldata) ->
#   showsdiv = $ '#nav-shows'
#   showsdiv.empty()
#
#   lb = ListBox.create
#      itemClass: ShowItem
#      items: alldata
#      mapFunction: "title"
#
#   lb.on "selectionChanged", ->
#      createNavEpisodes lb.selectedItem().episodes
#      clearDate()
#
#   lb.on "keydown", (e) ->
#      key = e.keyCode
#
#      switch key
#         when 39 # arrow right 
#            ep = $( '#nav-episodes .ListBox .EpisodeItem:first' ).control()
#            listbox = $( '#nav-episodes .ListBox' )
#            listbox.control().selectedControl ep
#            listbox.focus()
#
#   showsdiv.append lb


createNavEpisodes = (episodes) ->
   lb = $ "<ul class='mdl-list'></ul>"

   lb.append ( episodes.map (episode) ->
      li = $ "<li class='mdl-list__item'><span class='mdl-list__item-primary-content'>#{episode.title}</span></li>"
      li.click ->
         select li
         setDate "Episode timestamp: #{episode.date}"
      li.data 'playpath', episode.playpath
      return li
   )

   episodesDiv = $ '#nav-episodes'
   episodesDiv.empty()
   episodesDiv.append lb

# FIXME
#class EpisodeItem extends Control
#   title: (epdata) -> this.content epdata.title
#   playpath: Control.property()
#   date: Control.property()
#
#
#createNavEpisodes = (episodes) ->
#   episodesDiv = $ '#nav-episodes'
#   episodesDiv.empty()
#
#   episodesListBox = ListBox.create
#      itemClass: EpisodeItem
#      items: episodes
#      mapFunction: "title"
#
#   episodesListBox.on "selectionChanged", ->
#      sel = episodesListBox.selectedItem()
#      if sel
#         setDate "Episode acquired: #{sel.date}"
#      else
#         clearDate()
#
#   episodesListBox.on "keydown", (e) ->
#      key = e.keyCode
#
#      switch key
#         when 37 # arrow left
#            $( '#nav-episodes .ListBox' ).control().selectedControl null
#            $( '#nav-shows .ListBox' ).focus()
#         when 13, 32 # enter or space
#            playVideo episodesListBox.selectedItem()
#         when 46, 68 # delete or 'd' key
#            showDeleteDialog episodesListBox.selectedItem()
#
#   episodesDiv.append episodesListBox


playVideo = (playpath) -> console.log "Now playing: #{playpath}"
#playVideo = (ep) ->
#   $.ajax
#      url: 'playVideo'
#      type: 'POST'
#      data: ep.playpath
#      dataType: 'text'
#      contentType: 'text/plain'
#      #TODO: success: -> console.log 'success'


# FIXME Replace with MDL dialog
showDeleteDialog = (ep) ->
   DelConfDialog = Dialog.sub
      className: "DelConfDialog"
      inherited:
         content:
            [ "<h2>Confirm delete</h2>"
            , "<p>This item will be permanently deleted and cannot be recovered. Are you sure?</p>"
            , { control: BasicButton
              , ref: "buttonCancel"
              , content: "Cancel"
              , css: { "margin-top": "1em" }
              }
            , { control: BasicButton
              , ref: "buttonDelete"
              , content: "Delete"
              , css: { "margin-top": "1em", "margin-left": "0.5em", "border": "2px solid black" }
              }
            , css: { "align": "right" }
            ]
         width: "400px"

      initialize: ->
         self = this

         this.$buttonCancel().click -> self.cancel()

         this.$buttonDelete().click ->
            delEpisode ep
            self.close()

      cancel: ->
         @close()
         $( '#nav-episodes .ListBox' ).focus()

   Dialog.showDialog DelConfDialog
   ($ '.buttonDelete').focus()


delEpisode = (ep) ->
   $.ajax
      url: 'delVideo'
      type: 'POST'
      data: ep.playpath
      dataType: 'text'
      contentType: 'text/plain'
      success: -> loadNavShows()


define
   loadNavShows: loadNavShows
