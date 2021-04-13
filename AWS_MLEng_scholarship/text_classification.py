import fasttext
model = fasttext.load_model('lid.176.ftz')

model.predict('VARIMAX', k = 4)