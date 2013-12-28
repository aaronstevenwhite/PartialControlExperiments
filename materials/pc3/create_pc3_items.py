from random import shuffle
from string import Template

verbs = [('remembered', 'to'),
         ('regretted', 'ing'),
         ('deserved', 'to'),
         ('pretended', 'to'),
         ('liked', 'to'),
         ('hated', 'to'),
         ('liked', 'ing'),
         ('loved', 'ing'),
         ('hated', 'ing'),
         ('remembered', 'ing')]

noncollective = [(['participate', 'participating', 'participated'], 'in the book club'),
                 (['take', 'taking', 'took'], 'yoga classes'),
                 (['rent', 'renting', 'rented'], 'an apartment in Baltimore'),
                 (['hang', 'hanging', 'hung'], 'out at the mall')]

collective = [(tense_root, pred + ' together') for tense_root, pred in noncollective]

names = [('Sal', 'Harriet', 'Mike'),
         ('Val', 'Jan', 'Guy'),
         ('Jan', 'Jerry', 'Beth'),
         ('Carol', 'Ken', 'Jill'),
         ('Betty', 'Val', 'Barbara'),
         ('Matt', 'Erin', 'Bill'),
         ('Jerry', 'Carol', 'Hal'),
         ('Ken', 'Betty', 'Fred'),
         ('Erin', 'Sal', 'Rita'),
         ('Mel', 'Matt', 'Daphne'),
         ('Dan', 'Mel', 'Larry')]

sentences = {}

pred_types = {'collective'    : collective,
              'noncollective' : noncollective}

## create baseline sentences

base_temp = lambda name, pred: '{0} said that {1} {2} {3}.'.format(name[0], name[1], pred[0][2], pred[1])

sentences['base'] = {}

for ptype in pred_types:
    sentences['base'][ptype] = {pred[0][0] : [base_temp(name, pred)] for name, pred in zip(names, pred_types[ptype])}

## create test item template

cont_to_temp = lambda verb, name, pred: '{0} said that {1} {2} to {3} {4}.'.format(name[0], name[1], verb, pred[0][0], pred[1])
cont_ing_temp = lambda verb, name, pred: '{0} said that {1} {2} {3} {4}.'.format(name[0], name[1], verb, pred[0][1], pred[1])

## add test to sentences

for verb, comp_type in verbs:
    shuffle(names)
    sentences[verb+'_'+comp_type] = {}
    for ptype in pred_types:
        sentences[verb+'_'+comp_type][ptype] = {}
        for name, pred in zip(names, pred_types[ptype]):
            if comp_type == 'to':
                sentences[verb+'_to'][ptype][pred[0][0]] = [cont_to_temp(verb, name, pred)]
            else:
                sentences[verb+'_ing'][ptype][pred[0][0]] = [cont_ing_temp(verb, name, pred)]

## create filler item template

plr_temp = lambda verb, name, pred: '{0} said that {1} and {2} {3} to {4}.'.format(name[0], name[1], name[2], verb, pred)
ecm_temp = lambda verb, name, pred: '{0} said that {1} {2} {3} to {4}.'.format(name[0], name[1], verb, name[2], pred)
for_temp = lambda verb, name, pred: '{0} said that {1} {2} for {3} to {4}.'.format(name[0], name[1], verb, name[2], pred)

plr_verbs = ['remembered',
             'regretted']
ecm_verbs = ['deserved',
             'pretended',
             'liked',
             'hated']
for_verbs = ['loved',
             'regretted',
             'deserved',
             'pretended']

filler_preds = ['hang out at the mall together',
                'take yoga classes together',
                'participate in a book club',
                'rent an apartment in Baltimore alone',
                'take yoga classes alone',
                'participate in a book club',
                'rent an apartment in Baltimore',
                'hang out at the mall']

## add fillers to sentences

sentences['filler'] = {'filler' : {'filler' : []}}

for temp, verbs in [(plr_temp, plr_verbs), (ecm_temp, ecm_verbs), (for_temp, for_verbs)]:
    shuffle(names)
    for verb in verbs:
        for i, pred in enumerate(filler_preds):
            sentences['filler']['filler']['filler'].append(temp(verb, names[i], pred))


conf_temp = lambda items: Template('''
var shuffleSequence = seq("consent", "intro", "p", "begin", sepWith("sep", rshuffle(rshuffle("noncollective", "collective", "filler"))), "debrief");
var practiceItemTypes = ["p"];

var defaults = [
    "Separator", {
        transfer: 500,
        hideProgressBar: true,
        normalMessage: "+"
    },
    "Message", {
        hideProgressBar: true
    },
    "AcceptabilityJudgment", {
        q: '',
        as: ["1", "2", "3", "4", "5", "6", "7"],
        presentAsScale: true,
        instructions: "Use number keys or click boxes to answer.",
        leftComment: "(Bad)", rightComment: "(Good)"
    },
    "Form", { hideProgressBar: true }

];

var items = [
	["consent", "Form", {
        html: { include: "consent.html" },
		validators: {age: function (s) { if (s.match(/^\d+$$/)) return true;
							else return "Bad value for age"; }}
    } ],

	["intro", "Message", {html: { include: "intro.html" }}],

	["p", "AcceptabilityJudgment", {s: "Gary ran that he ate a piece of cake."}],
	["p", "AcceptabilityJudgment", {s: "Stacy built a house for him."}],
	["p", "AcceptabilityJudgment", {s: "I failed going to the store."}],

	["begin", "Message", {
				html: { include: "begin.html" },
				} ],

    ["sep", "Separator", { }],

    $items

];
''').substitute(items=items)

item_temp = lambda typ, sent: Template('["$typ", "AcceptabilityJudgment", {s: \'$sent\'}]').substitute(typ=typ, sent=sent)

items = []
items_conf = []

f = open('pc.sents', 'w')

for verb in sentences:
    for ptype in sentences[verb]:
        for pred in sentences[verb][ptype]:
            for sent in sentences[verb][ptype][pred]:
                f.write(sent+'\n')
                vtype = '_'.join(verb.split())
                items_conf.append('\t'.join([vtype, ptype, pred]))
                items.append(item_temp(ptype, sent))

f.close()
                
for i in range(24):
    items.append(Template('[["debrief", 1], "Form", {html: { include: "debrief${num}.html" }}]').substitute(num=str(i)))

f = open('pc_items.js', 'w')
g = open('pc_items.conf', 'w')

f.write(conf_temp(',\n\t'.join(items)))
g.write('\n'.join(items_conf))

f.close()
g.close()
