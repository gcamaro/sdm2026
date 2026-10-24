from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import unquote,urlsplit
import re,json,html
R=Path(__file__).resolve().parents[1]
class Page(HTMLParser):
 def __init__(self):super().__init__();self.links=[];self.ids=set()
 def handle_starttag(self,t,attrs):
  a=dict(attrs)
  if 'id' in a:self.ids.add(a['id'])
  for k in ['href','src']:
   if k in a:self.links.append(a[k])
errors=[];pages={}
for f in R.glob('*.html'):
 p=Page();p.feed(f.read_text());pages[f.name]=p
for name,p in pages.items():
 for link in p.links:
  u=urlsplit(link)
  if u.scheme or u.netloc:continue
  dest=R/unquote(u.path) if u.path else R/name
  if not dest.exists():errors.append(f'{name}: arquivo ausente {link}')
  elif u.fragment and dest.suffix=='.html' and u.fragment not in pages.get(dest.name,Page()).ids:errors.append(f'{name}: seção ausente {link}')
snippets=[]
for f in sorted((R/'capitulos').glob('*.json')):
 c=json.loads(f.read_text())
 for i,s in enumerate(re.findall(r'<pre><code[^>]*>(.*?)</code></pre>',c['body'],re.S)):
  snippets.append((c['slug'],i,html.unescape(s)))
T=Path('/tmp/sdm-book-code-check');T.mkdir(exist_ok=True)
for slug,i,s in snippets:(T/f'{slug}-{i}.R').write_text(s)
print(f'{len(pages)} páginas; links e âncoras locais verificados; {len(snippets)} trechos exportados para análise de sintaxe.')
if errors:raise SystemExit('\n'.join(errors))
print('OK')
