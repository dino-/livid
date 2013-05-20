loadNavShows = () ->
   req = $.getJSON '/getShowList', (data) ->
      createNavShows data
      createNavEpisodes []
      sizeContent()
      ($ window).resize sizeContent
      document.body.style.visibility = 'visible'
      selectFirstShow()
   req.error (resp) -> ($ 'body').html resp.responseText


selectFirstShow = () ->
   first = ($ '#nav-shows .ListBox .ShowItem:first').control()
   listbox = ($ '#nav-shows .ListBox')
   listbox.control().selectedControl first
   listbox.focus()


sizeContent = () ->
   contentHeight = window.innerHeight -
      ($ '#header').height() -
      ($ '#footer').height()
   ($ '#nav-container').height contentHeight


class ShowItem extends Control
   title: (showdata) -> this.content showdata.title
   episodes: Control.property()


createNavShows = (alldata) ->
   showsdiv = $ '#nav-shows'

   lb = ListBox.create
      itemClass:  ShowItem
      items:  alldata
      mapFunction: "title"

   lb.on "selectionChanged", ->
      createNavEpisodes lb.selectedItem().episodes

   lb.on "keydown",  (e) ->
      key = e.keyCode

      switch key
         when 39 # arrow right 
            ep = $( '#nav-episodes .ListBox .EpisodeItem:first' ).control()
            $( '#nav-episodes .ListBox' ).control().selectedControl ep
            $( '#nav-episodes .ListBox' ).focus()

   showsdiv.append lb


class EpisodeItem extends Control
   title: (epdata) -> this.content epdata.title
   playpath: Control.property()
   date: Control.property()


createNavEpisodes = (episodes) ->
   episodesDiv = $ '#nav-episodes'
   episodesDiv.empty()

   episodesListBox = ListBox.create
      itemClass: EpisodeItem
      items: episodes
      mapFunction: "title"

   episodesListBox.on "keydown",  (e) ->
      key = e.keyCode

      switch key
         when 37 # arrow left
            $( '#nav-episodes .ListBox' ).control().selectedControl null
            $( '#nav-shows .ListBox' ).focus()
         when 13, 32 # enter or space
            playVideo episodesListBox.selectedItem()
         when 46, 68 # delete or'd' key
            console.log 'handle delete key'
            #showDeleteDialog episodesListBox.selectedItem()
            #showDeleteDialog this, epdata

   episodesDiv.append episodesListBox


playVideo = (ep) ->
   $.ajax
      url: 'playVideo'
      type: 'POST'
      data: ep.playpath
      dataType: 'text'
      contentType: 'text/plain'
      #TODO: success: () -> console.log 'success'


showDeleteDialog = (li, ep) ->
   ($ '#del-confirm-dialog' ).dialog
      width: 400
      modal: true
      close: () -> ($ li).focus()
      buttons:
         Delete: () ->
            delEpisode ep
            ($ this).dialog 'close'

         Cancel: () ->
            ($ this).dialog 'close'

   ($ '#del-confirm-dialog' ).dialog 'open'


delEpisode = (ep) ->
   $.ajax
      url: 'delVideo'
      type: 'POST'
      data: ep.playpath
      dataType: 'text'
      contentType: 'text/plain'
      success: () -> loadNavShows()


define
   loadNavShows: loadNavShows
