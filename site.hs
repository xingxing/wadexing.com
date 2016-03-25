--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "static/*/*" $ do
                  route idRoute
                  compile copyFileCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" siteCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    siteCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    siteCtx 

siteCtx :: Context String
siteCtx = 
    constField "baseurl" "http://www.wadexing.com" `mappend` 
    constField "site_description" "安识紫霞客，瑶台鸣素琴" `mappend`
    constField "twitter_username" "wadexing" `mappend`
    constField "github_username" "xingxing" `mappend`
    defaultContext

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 65022' \
                      \_site/* \
                      \xing@wadexing.com:/home/xing/blog"
    }

--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Wade Xing's blog"
    , feedDescription = "This feed provides nothing!"
    , feedAuthorName  = "Wade Xing"
    , feedAuthorEmail = "iamxingxing@gmail.com"
    , feedRoot        = "http://www.wadexing.com"
    }
