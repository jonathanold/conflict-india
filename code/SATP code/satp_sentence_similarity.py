from sentence_transformers import SentenceTransformer, util
import spacy
import pandas as pd
import re

class EventCategorizer:
    def __init__(self):
        # Load pre-trained BERT model and spaCy model
        self.model = SentenceTransformer('bert-base-nli-mean-tokens')
        self.nlp = spacy.load("en_core_web_sm")

        # Define categories and template sentences
        self.categories = {
            1: "Police or security forces arrest rebels/terrorists",
            2: "Police or security forces attack rebels/terrorists",
            3: "Encounter between police/security forces and rebels/terrorists",
            4: "Rebel/terrorist surrenders",
            5: "Police or security forces make a raid and secure weapons or other items",
            6: "Rebels/terrorists attack civilians",
            7: "Rebels/terrorists attack police or security forces",
            8: "Rebels/terrorists attack infrastructure",
            9: "Statement by politician about conflict situation",
            10: "Civilians protest peacefully",
            11: "Civilians riot or are violent in another way",
            12: "Politicians reach out to rebels, try to reintegrate them",
            13: "Former rebel/terrorist runs for political office",
            14: "Current rebel/terrorist runs for political office",
            15: "Politicians receives threats from rebels/terrorists",
            16: "Police or security forces attack civilians",
            17: "Government announces policy to reduce rebel activity, such as development programme, peace talks, or amnesties",
            18: "Clash between Indian Security Forces and foreign Security Forces",
            19: "Clash between two different (rivaling) rebel groups"
        }

        self.category_templates = {
            1: ["Police arrested a rebel", "Security forces apprehended terrorists", "Authorities arrested a Maoist cadre"],
            2: ["Police attacked rebel hideout", "Security forces engaged with terrorists", "Security forces launched an operation against militants"],
            3: ["Police clashed with rebels", "Security forces encountered terrorists", "Security forces and militants exchanged fire"],
            4: ["A rebel surrendered to authorities", "A terrorist turned himself in", "Maoist cadres surrendered before the police"],
            5: ["Police raided a hideout and seized weapons", "Security forces conducted a raid and recovered arms", "Security forces found explosives during a search operation"],
            6: ["Rebels attacked a village", "Terrorists opened fire on civilians", "Militants killed civilians"],
            7: ["Rebels ambushed a police patrol", "Terrorists targeted security forces", "Militants attacked security personnel"],
            8: ["Rebels bombed a bridge", "Terrorists attacked a power plant", "Militants triggered an explosive device"],
            9: ["The Prime Minister commented on the conflict", "A politician spoke about the security situation", "A statement was issued by a leader regarding the insurgency"],
            10: ["Civilians held a peaceful protest", "People demonstrated against the government"],
            11: ["Civilians rioted in the streets", "Protesters turned violent and clashed with police"],
            12: ["The government reached out to rebels for peace talks", "Politicians offered amnesty to insurgents"],
            13: ["A former rebel contested elections", "An ex-terrorist ran for political office"],
            14: ["A rebel leader announced his candidacy", "A terrorist stood for elections"],
            15: ["A politician received threats from rebels", "Terrorists warned a leader against campaigning"],
            16: ["Security forces fired on protesters", "Police attacked civilians during a protest"],
            17: ["The government announced a development program to curb insurgency", "New policies were unveiled to address rebel activity"],
            18: ["Indian and Pakistani troops exchanged fire", "Security forces from two countries clashed at the border"],
            19: ["Two rebel groups fought each other", "Rival terrorist factions engaged in infighting"]
        }

        # Define rule-based patterns
        self.event_patterns = {
            r"police\s+arrest.*rebel|terrorist|maoist|naxal|cadre": 1,
            r"securit(y\s+force|ies)\s+attack.*rebel|terrorist|maoist|naxal|militant": 2,
            r"encounter\s+between\s+securit(y\s+force|ies)\s+and\s+rebel|terrorist|maoist|naxal|militant": 3,
            r"rebel|terrorist|maoist|naxal|cadre\s+surrender(ed|s)": 4,
            r"securit(y\s+force|ies)\s+raid.*seized|recover.*weapons|arms|explosives": 5,
            r"rebel|terrorist|maoist|naxal|militant\s+attack.*civilian|village|people": 6,
            r"rebel|terrorist|maoist|naxal|militant\s+attack.*securit(y\s+force|ies)|police": 7,
            r"rebel|terrorist|maoist|naxal|militant\s+attack.*infrastructure|bridge|plant|device": 8,
            r"statement|comment.*politician|leader|minister\s+conflict|situation|insurgency": 9,
            r"civilian\s+protest.*peaceful": 10,
            r"civilian\s+riot|violent|clash.*police": 11,
            r"government|politician\s+reach\s+out\s+rebel|terrorist|maoist|naxal\s+peace|amnesty": 12,
            r"former\s+rebel|terrorist|maoist|naxal\s+election|office": 13,
            r"current\s+rebel|terrorist|maoist|naxal\s+election|office": 14,
            r"politician|leader\s+threat.*rebel|terrorist|maoist|naxal": 15,
            r"securit(y\s+force|ies)|police\s+attack.*civilian|protest": 16,
            r"government\s+announce.*polic(y|ies)\s+reduce\s+rebel|terrorist|maoist|naxal\s+activit(y|ies)": 17,
            r"indian\s+securit(y\s+force|ies)\s+and\s+foreign\s+securit(y\s+force|ies)\s+clash": 18,
            r"rebel|terrorist\s+group\s+clash.*rival|different": 19
        }

    def calculate_similarity(self, sentence1, sentence2):
        embeddings = self.model.encode([sentence1, sentence2])
        similarity_score = util.pytorch_cos_sim(embeddings[0], embeddings[1])
        return similarity_score.item()

    def extract_entities(self, sentence):
        doc = self.nlp(sentence)
        entities = {}
        for ent in doc.ents:
            entities[ent.label_] = ent.text
        return entities

    def categorize_event(self, event_description):
        best_score = 0
        best_category = None
        for category, templates in self.category_templates.items():
            category_scores = [self.calculate_similarity(event_description, template) for template in templates]
            category_score = max(category_scores)
            if category_score > best_score:
                best_score = category_score
                best_category = category

        # Incorporate rule-based patterns
        for pattern, cat_id in self.event_patterns.items():
            if re.search(pattern, event_description, re.IGNORECASE):
                best_category = cat_id
                break

        # Handle unknown categories
        if best_category is None:
            best_category = "Unknown"

        return best_category

categorizer = EventCategorizer()

# Load preprocessed SATP CSV
file_path = r'C:\Users\Jade Chen\Downloads\dataset_satp_clean_for_similarity.csv'
satp = pd.read_csv(file_path)

# Apply categorizer to first 30 subsets so my laptop does not die
satp_subset = satp.head(30)
satp_subset['category'] = satp_subset['descr_short'].apply(categorizer.categorize_event)

# Display categorized events
print(satp_subset[['descr_short', 'category']])

# How many occurrences are there for each category?
category_counts = satp_subset['category'].value_counts()

# What is the total number of events?
total_events = len(satp_subset)

# What is the ratio of events in each category?
category_ratios = {category: count / total_events for category, count in category_counts.items()}

# Display ratio of events in each category
for category, ratio in category_ratios.items():
    print(f"Category {category}: {ratio:.2f}")