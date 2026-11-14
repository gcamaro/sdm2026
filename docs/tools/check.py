"""Check generated links, scope and script excerpts without running the model."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit,urljoin,unquote
import json,re,html,hashlib
R=Path(__file__).resolve().parents[1]
class Page(HTMLParser):
 def __init__(self):super().__init__();self.refs=[];self.ids=set();self.base='';self.downloads=0
 def handle_starttag(self,tag,attrs):
  a=dict(attrs)
  if tag=='base':self.base=a.get('href','')
  if a.get('id'):self.ids.add(a['id'])
  self.downloads+='download' in a
  if tag!='base':
   for k in ('href','src'):
    if k in a:self.refs.append(a[k])
errors=[];pages={}
for f in R.rglob('*.html'):
 p=Page();p.feed(f.read_text());pages[f]=p
for f,p in pages.items():
 if p.downloads:errors.append(f'{f.name}: automatic download')
 base=urljoin(f.as_uri(),p.base)
 for link in p.refs:
  u=urlsplit(urljoin(base,link))
  if u.scheme!='file':continue
  target=Path(unquote(u.path))
  if not target.exists():errors.append(f'{f.relative_to(R)}: missing {link}')
  elif u.fragment and target in pages and u.fragment not in pages[target].ids:errors.append(f'{f.relative_to(R)}: missing anchor {link}')
blocks=0;literal=0;max_lines=0
for f in sorted((R/'capitulos').glob('*.json')):
 c=json.loads(f.read_text())
 if not re.match(r'(?:0[1-9]|1[0-6])-',c['slug']):continue
 if re.search(r'Pyrophyta|Amanoa|esm_(?:max|glm|gam)',c['body'],re.I):errors.append(c['slug']+': outside main workflow')
 for card in c['body'].split('<div class="code-card">')[1:]:
  found=re.search(r'<pre><code[^>]*>(.*?)</code>',card,re.S)
  if not found:continue
  text=html.unescape(found[1]);n=len(text.splitlines());blocks+=1;max_lines=max(max_lines,n)
  if n>9:errors.append(c['slug']+': long code block')
  loc=re.search(r'trecho original · linhas (\d+)–(\d+)',card)
  if loc:
   a,b=map(int,loc.groups());lines=(R/'fontes/scripts/Zaprionus tuberculatus SDM e Futuro 2026-10-12.R').read_text().splitlines()
   if text!='\n'.join(lines[a-1:b]):errors.append(c['slug']+': excerpt differs from source')
   literal+=1
license=json.loads((R/'capitulos/18-fontes-e-reproducao.json').read_text())
if license['title']!='Licença' or not license.get('simple'):errors.append('License page is not simple')
if 'george.amaro@embrapa.br' not in license['body'] or 'https://creativecommons.org/licenses/by-nc/4.0/' not in license['body']:errors.append('License details missing')
refs=json.loads((R/'capitulos/19-referencias.json').read_text())
if 'Outras obras e materiais citados' in refs['body']:errors.append('Excluded references section reintroduced')
manifest=json.loads((R/'fontes/manifesto.json').read_text())
for s in manifest['sources']:
 if s['name'].endswith('.R'):
  f=R/'fontes/scripts'/s['name']
  if hashlib.sha256(f.read_bytes()).hexdigest()!=s['sha256']:errors.append('Script hash differs')
print(f'{len(pages)} pages; {blocks} R blocks; {literal} literal excerpts; maximum {max_lines} lines per block.')
if errors:raise SystemExit('\n'.join(errors))
print('OK: internal links, anchors, scope, license and source excerpts.')
