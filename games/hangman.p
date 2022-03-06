; hangman.

(import :rand)
(<- $dict
    '(again angry apple apron beach black blood boots bread broom brown camel candy catch
      chair cheek chest chill class clean clerk clock cloud cocoa color coral crane curve
      dance diary dream drink eagle early earth eight elbow enjoy every field floor front
      funny glass glove goose grape grass green happy horse hotel house jelly juice knife
      koala later laugh learn leave lemon light lunch melon meter month mouse mouth music
      night noisy north nurse often onion organ panda paper paste peach piano pilot pizza
      place plant plate purse quiet right river ruler scale seven shark sheep shell shoes
      short skirt skunk sleep smart smile smoke snake socks sorry south space spoon sport
      stamp stand stone study sugar sweat table tears their there these thick thing those
      three thumb tiger today towel train uncle under visit watch water whale where which
      white whose women write yacht young yours zebra)
    $word nil
    $state nil
    $history nil)

(function update-state ()
  (<- $state (map (f (ch)
                    (if (in? ch $history) ch
                        "*"))
                  $word)))

(function input ()
  (write-bytes ") ")
  (let (line (lower (read-line)))
    (if (|| (!= (len line) 1) (! (alpha? line)) (in? line $history))
        (begin
          (write 'again)
          (input))
        (begin0
          (push! line $history)
          (update-state)))))

(function init ()
  (rand.seed (time))
  (<- $word (split (str (rand.choice $dict)))
      $history nil)
  (update-state))

(function! main (args)
  (let (life 7 ch nil)
    (init)
    (while (> life 0)
      (write-line (join $state))
      (write-line (str "life: " life " " (if $history (join $history ", "))))
      (if (! (in? (<- ch (input)) $word)) (<- life (-- life))
          (! (in? "*" $state)) (break)))
    (write-line (join $word))
    (if (= life 0) (write 'lose)
        (write 'win))))
