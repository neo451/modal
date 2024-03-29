
import split from require"pl.stringx"
require"moon.all"
githubPath = (base, subpath = '') ->
  -- if (!base.startsWith('github:')) {
  --   throw new Error('expected "github:" at the start of pseudoUrl');
  -- }
  { _, path } = split(base, 'github:')
  -- path = path.endsWith('/') ? path.slice(0, -1) : path
  if (#split(path, '/') == 2) 
    -- assume main as default branch if none set
    path ..= '/main'

  return ("https://raw.githubusercontent.com/%s/%s")\format(path, subpath)

p githubPath('github:yaxu/clean-breaks')

-- os.execute("curl " .. "https://mirror.ghproxy.com/" .. githubPath('github:yaxu/clean-breaks'))
