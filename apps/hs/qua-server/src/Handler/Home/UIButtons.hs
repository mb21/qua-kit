{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Home.UIButtons
  ( uiButtons
  ) where

import Import
import Text.Julius

-- | Rotating circular progress bar.
--   Exposes:
--     * #helpbutton
--     * #fullscreenbutton
--     * #submitbutton
--     * #toolboxbutton
--     * #clearbutton
--     * #evaluatebutton
uiButtons :: Maybe (ScenarioProblemId, UserId) -> Handler (Widget, Widget)
uiButtons mscpuid = do

  popupSubmitId <- newIdent
  popupShareId <- newIdent
  sfGeometry <- newIdent
  sfPreview <- newIdent
  submitForm <- newIdent
  previewcontainer <- newIdent

  textAreaDesc <- newIdent

  let popupSubmit = do
        toWidgetBody
              [hamlet|
                <script>
                  (function() {
                  if (document.getElementById("facebook-jssdk")) return;
                  var fbdiv = document.createElement("div");
                  fbdiv.id = "fb-root";
                  document.body.insertBefore(fbdiv, document.body.firstChild);
                  var js = document.createElement("script");
                  js.id = "facebook-jssdk";
                  js.src = "//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8&appId=1837721753181850";
                  document.body.insertBefore(js, document.body.firstChild);}());

                <div style="display: none;" aria-hidden="true" class="modal modal-va-middle fade" ##{popupSubmitId} role="dialog" tabindex="-1">
                  <div class="modal-dialog modal-xs">
                    <div class="modal-content">
                      <div class="modal-heading">
                        <p class="modal-title">
                          Submit your design
                      <div class="modal-inner">
                        <div ##{previewcontainer}>
                        <form ##{submitForm} method="post">
                          <input type="hidden" ##{sfGeometry} name="geometry">
                          <input type="hidden" ##{sfPreview} name="preview">
                          <div class="form-group form-group-label">
                            <label.floating-label for="#{textAreaDesc}">Share your ideas
                            <textarea.form-control.textarea-autosize form="#{submitForm}" id="#{textAreaDesc}" rows="1" name="description">
                      <div class="modal-footer">
                        <p class="text-right">
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal">
                            Cancel
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal" onclick="$('##{submitForm}').submit();">
                            Save


                $case mscpuid
                  $of Nothing
                  $of Just (pId, uId)
                    <div style="display: none;" aria-hidden="true" class="modal modal-va-middle fade" ##{popupShareId} role="dialog" tabindex="-1">
                      <div class="modal-dialog modal-xs">
                        <div class="modal-content">
                          <div class="modal-heading">
                            <p class="modal-title">
                              Share this design with others
                          <div class="modal-inner">
                            <p class="text">
                              You can share the following link that refers to the last saved version of this design:
                            <code>
                              @{SubmissionViewerR pId uId}
                            <p class="text">
                              Alternatively, use your favourite button:
                            <div style="text-align:center">
                              <a.shareButton onclick="FB.ui({method: 'share',mobile_iframe: true, href: '@{SubmissionViewerR pId uId}'}, function(response){});">
                                <img src="@{StaticR img_fbIcon_png}" style="width:40px;height:40px;" title="Share on Facebook" alt="Share on Facebook">
                              <a.shareButton href="http://vk.com/share.php?url=@{SubmissionViewerR pId uId}" onclick="javascript:window.open(this.href, 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;">
                                <img src="@{StaticR img_vkIcon_png}" style="width:40px;height:40px;" title="Share on Vkontakte" alt="Share on Vkontakte">
                              <a.shareButton onclick="window.open('https://twitter.com/intent/tweet?url=' + encodeURIComponent('@{SubmissionViewerR pId uId}') + '&text=' + encodeURIComponent('Check out this design on #quakit!') + '&hashtags=mooc,edx,ethz,chairia,urbandesign', 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;">
                                <img src="@{StaticR img_twitterIcon_png}" style="width:40px;height:40px;" title="Tweet the link" alt="Tweet the link">
                              <a.shareButton href="https://plus.google.com/share?url=@{SubmissionViewerR pId uId}" onclick="javascript:window.open(this.href, 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;">
                                <img src="@{StaticR img_gIcon_png}" style="width:40px;height:40px;" title="Share on Google" alt="Share on Google">
                          <div class="modal-footer">
                            <p class="text-right">
                              <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal">
                                Close
              |]
      uiButtonsG = do
        uiButton <- newIdent
        guiElement <- newIdent
        guiplaceholder <- newIdent
        idleplaceholder <- newIdent
        activeplaceholder <- newIdent
        btn <- newIdent

        serviceClear <- newIdent
        serviceRun <- newIdent
        submitbutton <- newIdent

        toWidgetHead
          [cassius|
            /* GUI icons */

            .#{guiElement}
                fill: #FF8A65 /* #FFAB91 */
                stroke: #FFFFFF
                stroke-width: 0.0

            .#{uiButton}:hover
                cursor: pointer

            .#{uiButton}:hover .#{guiElement}
                fill: #FF5722

            .#{uiButton}:active .#{guiElement}
                fill: #FF5722

            .#{uiButton}
                position: relative
                padding: 0
                margin: 0
                z-index: 10
                overflow: hidden
                width: 64px
                height: 64px

            rect.#{btn}
                stroke: #FFFFFF
                fill: #FFFFFF
                fill-opacity: 0
                stroke-opacity: 0

            ##{guiplaceholder}
                position: absolute
                bottom: 0
                padding: 0
                margin: 0
                z-index: 4
                overflow: visible
            /*    height: 256px */
                width: 64px
                -webkit-transition: width 300ms ease-in-out, left 300ms ease-in-out
                -moz-transition: width 300ms ease-in-out, left 300ms ease-in-out
                -o-transition: width 300ms ease-in-out, left 300ms ease-in-out
                transition: width 300ms ease-in-out, left 300ms ease-in-out

            .#{activeplaceholder}
                left: -32px

            .#{idleplaceholder}
                left: -64px

            .shareButton
                display: inline-block
                width: 40px
                height: 40px
                cursor: pointer
                cursor: hand
                -webkit-filter: drop-shadow(0px 1px 3px rgba(0,0,0,.3))
                filter: drop-shadow(0px 1px 3px rgba(0,0,0,.3))
                -ms-filter: "progid:DXImageTransform.Microsoft.Dropshadow(OffX=0, OffY=1, Color='#444')"
                filter: "progid:DXImageTransform.Microsoft.Dropshadow(OffX=0, OffY=1, Color='#444')"
          |]
--            sfGeometry <- newIdent
--  sfPreview <- newIdent
--  submitButton <- newIdent
--  previewcontainer <- newIdent
        toWidgetHead
          [julius|
            "use strict"
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: (submitUrl -> FeatureCollection -> Image -> IO ()) -> IO ()
             *  return :: IO ()
             */
            function registerSubmit(onClick) {
              document.getElementById('#{rawJS submitbutton}').style.display = "block"
              document.getElementById('#{rawJS submitbutton}').addEventListener("click", function() {
                  onClick( function(submitUrl, fc, img) {
                    document.getElementById('#{rawJS sfGeometry}').value = JSON.stringify(fc);
                    document.getElementById('#{rawJS sfPreview}').value = img;
                    var eimg = document.createElement("img");
                    eimg.src = img;
                    eimg.style.width = "100%";
                    document.getElementById('#{rawJS previewcontainer}').innerHTML = "";
                    document.getElementById('#{rawJS previewcontainer}').appendChild(eimg);
                    document.getElementById('#{rawJS submitForm}').action = submitUrl;
                    $('##{rawJS popupSubmitId}').modal('show');
                  });
                });
            }
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: IO ()
             *  return :: IO ()
             */
            function registerResetCamera(onClick) {
              document.getElementById('resetposbutton').addEventListener("click", onClick);
            }
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: IO ()
             *  return :: IO ()
             */
            function registerServiceClear(onClick) {
              document.getElementById('#{rawJS serviceClear}').addEventListener("click", onClick);
            }
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: IO ()
             *  return :: IO ()
             */
            function registerServiceRun(onClick) {
              document.getElementById('#{rawJS serviceRun}').style.display = "block"
              document.getElementById('#{rawJS serviceRun}').addEventListener("click", onClick);
            }
            /** Shows or hides button "clear"; comes from Handler.Home.UIButtons.
             *  state :: Bool
             *  return :: IO ()
             */
            function toggleServiceClear(state) {
              document.getElementById('#{rawJS serviceClear}').style.display = state ? "block" : "none";
              document.getElementById('#{rawJS serviceRun}').style.display = state ? "none" : "block";
            }
            // Toggle fullscreen and change the fullscreen button shape
            function toggleFullScreen() {
                if (!document['fullscreenElement'] && !document['mozFullScreenElement']
                 && !document['webkitFullscreenElement'] && !document['msFullscreenElement'] && !document['fullScreen']) {
                  if (document.documentElement['requestFullscreen']) {
                    document.documentElement.requestFullscreen();
                  } else if (document.documentElement['msRequestFullscreen']) {
                    document.documentElement.msRequestFullscreen();
                  } else if (document.documentElement['mozRequestFullScreen']) {
                    document.documentElement.mozRequestFullScreen();
                  } else if (document.documentElement['webkitRequestFullscreen']) {
                    document.documentElement.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT);
                  }
                } else {
                  if (document['exitFullscreen']) {
                    document.exitFullscreen();
                  } else if (document['msExitFullscreen']) {
                    document.msExitFullscreen();
                  } else if (document['mozCancelFullScreen']) {
                    document.mozCancelFullScreen();
                  } else if (document['webkitExitFullscreen']) {
                    document.webkitExitFullscreen();
                  } else {
                    document.cancelFullScreen();
                    document.exitFullscreen();
                  }
                }
            };

            // change fullscreen button
            function checkfullscreen() {
                if (document['fullscreenElement'] || document['webkitFullscreenElement'] || document['mozFullScreenElement']) {
                    document.getElementById('fullscreenbicon').innerText = "fullscreen_exit";
                } else {
                    document.getElementById('fullscreenbicon').innerText = "fullscreen";
                }
            };

            document.addEventListener('webkitfullscreenchange', function(e) {checkfullscreen();}, false);
            document.addEventListener('mozfullscreenchange', function(e) {checkfullscreen();}, false);
            document.addEventListener('msfullscreenchange', function(e) {checkfullscreen();}, false);
            document.addEventListener('fullscreenchange', function(e) {checkfullscreen();}, false);

            // Toggle GUI panel on the right side of window
            function toggleGUIPanel(){
                var panel = document.getElementById('guipanel');
                if (panel.className == 'idleguipanel') {
                    panel.className = 'activeguipanel';
                    document.getElementById('#{rawJS guiplaceholder}').className = '#{rawJS activeplaceholder}';
                } else {
                    panel.className = 'idleguipanel';
                    document.getElementById('#{rawJS guiplaceholder}').className = '#{rawJS idleplaceholder}';
                }
            };
          |]
        toWidgetBody
          [hamlet|
            <div .#{idleplaceholder} ##{guiplaceholder}>
              <div class="fbtn-inner open">
                <a aria-expanded="true" class="fbtn fbtn-lg fbtn-red waves-attach waves-circle waves-light waves-effect" onclick="$(this).parent().toggleClass('open');">
                  <span class="fbtn-text fbtn-text-left">Tools
                  <span class="fbtn-ori icon">apps
                  <span class="fbtn-sub icon">close
                <div class="fbtn-dropup">
                  $case mscpuid
                    $of Just (_, _)
                      <a class="fbtn fbtn-brand waves-attach waves-circle waves-effect" onclick="$('##{popupShareId}').modal('show');">
                        <span class="fbtn-text fbtn-text-left">Share
                        <span class="icon icon-lg">share
                    $of Nothing
                  <a class="fbtn waves-attach waves-circle waves-effect fbtn-brand-accent" #resetposbutton>
                    <span class="fbtn-text fbtn-text-left">Reset camera position
                    <span class="icon icon-lg" style="font-size: 2em;margin-left:-8px;vertical-align:-32%;margin-top:-3px;">fullscreen
                    <span class="icon icon" style="margin-left: -24px;font-size: 1em;line-height: 1em;">videocam
                  <a class="fbtn waves-attach waves-circle waves-effect" #helpbutton onclick="$('#popuphelp').modal('show')">
                    <span class="fbtn-text fbtn-text-left">How-to: mouse & finger controls
                    <span class="icon icon-lg">help_outline
                  <a class="fbtn waves-attach waves-circle waves-effect" #fullscreenbutton onclick="toggleFullScreen()">
                    <span class="fbtn-text fbtn-text-left">Toggle fullscreen
                    <span class="icon icon-lg" #fullscreenbicon>fullscreen
                  <a class="fbtn waves-attach waves-circle waves-effect" #toolboxbutton onclick="toggleGUIPanel()">
                    <span class="fbtn-text fbtn-text-left">Control panel
                    <span class="icon icon-lg">settings
                  <a class="fbtn fbtn-brand waves-attach waves-circle waves-effect" style="display: none;" ##{submitbutton}>
                    <span class="fbtn-text fbtn-text-left">Submit proposal
                    <span class="icon icon-lg">save
                  <a class="fbtn fbtn-brand-accent waves-attach waves-circle waves-light waves-effect" style="display: none;" ##{serviceClear}>
                    <span class="fbtn-text fbtn-text-left">Clear service results
                    <span class="icon icon-lg">visibility_off
                  <a class="fbtn fbtn-green waves-attach waves-circle waves-effect" style="display: none;" ##{serviceRun}>
                    <span class="fbtn-text fbtn-text-left">Run evaluation service
                    <span class="icon icon-lg">play_arrow
          |]
  return (uiButtonsG, popupSubmit)
