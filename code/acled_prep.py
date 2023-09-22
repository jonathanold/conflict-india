import pandas as pd
import nltk  # https://realpython.com/nltk-nlp-python/#chunking
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer, PorterStemmer
from nltk.corpus import stopwords
from nltk import RegexpParser

file_path = r'C:\Users\Jade Chen\URAP_India\conflict-india\data\acled_india_30may.csv'
acled_df = pd.read_csv(file_path, delimiter=';')

# Display the first five rows and data types of each column
print(acled_df.head(5))
print(acled_df.dtypes)

# Display the 'notes' column
print(acled_df['notes'].head())


# Define a function to prepare text for NLP
def prep_text(text):
    if isinstance(text, str):
        # Tokenize the text into words
        words = word_tokenize(text)

        # Remove punctuation and convert to lowercase
        words = [word.lower() for word in words if word.isalnum()]

        # Remove stopwords using the "english" set of stopwords
        stop_words = set(stopwords.words("english"))
        words = [word for word in words if word not in stop_words]

        # Lemmatizing (reduce words to their core meaning)
        lemmatizer = WordNetLemmatizer()
        words = [lemmatizer.lemmatize(word) for word in words]

        # Stemming (reduce words to their root)
        stemmer = PorterStemmer()
        words = [stemmer.stem(word) for word in words]

        # Chunking (group words to identify phrases)
        # NOTE TO SELF: change chunk grammar regexes depending on project next steps
        grammar = r"""
            NP: {<DT>?<JJ>*<NN>} # Chunk noun phrases
            VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verb phrases
        """
        chunk_parser = RegexpParser(grammar)
        tree = chunk_parser.parse(nltk.pos_tag(words))  # parses the POS-tagged words into chunks

        # Chinking (excludes some pattern from chunks)
        chink_parser = RegexpParser("NP: {<.*>+} # Chink all else")
        tree = chink_parser.parse(tree)  # Use tree.draw() to visualize tree if needed

        # Convert the parsed tree back to a string
        cleaned_text = ' '.join([word for subtree in tree for word in subtree if isinstance(word, str)])

        return cleaned_text

    # If text is NaN, return an empty string
    else:
        return ''


# Apply the prep_text function to the 'notes' column and store in new 'cleaned_notes' column
acled_df['cleaned_notes'] = acled_df['notes'].apply(prep_text)

# Display the 'cleaned_notes' and 'notes' column
print(acled_df[['notes', 'cleaned_notes']].head())
