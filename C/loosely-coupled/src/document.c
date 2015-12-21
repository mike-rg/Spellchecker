#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "document.h"

struct sDocument {
    FILE *doc_in;
    FILE *doc_out;
};
/*****************************************************************************/

int doc_get_word(char *word, Document document) {
    
    char *end_of_str = "\0";
    int character_readed, i = 0;

    while (!feof(document->doc_in)) {
        character_readed = fgetc(document->doc_in);

        if (isalpha(character_readed)) {
            word[i] = character_readed;
        } else {
            word[i] = *end_of_str;
            if (character_readed != EOF) {
                if (i == 0) {
                    fprintf(document->doc_out, "%c", character_readed);
                }
                else {
                    fseek(document->doc_in, -1, SEEK_CUR);
                }
                return 1;
            }
        }
        i++;
    }
    return 0;
}
/*****************************************************************************/

Document doc_open(char *doc_in, char* doc_out) {
    assert(doc_out != NULL);
    assert(doc_in != NULL);
    Document new_document = calloc(1, sizeof(struct sDocument));
    new_document->doc_in = fopen(doc_in, "r");
    new_document->doc_out = fopen(doc_out, "w");

    if (!new_document->doc_in || !new_document->doc_out) {
        printf("ERROR: couldn't open document\n");
        free(new_document);
        exit(EXIT_FAILURE);
    }
    return new_document;
}
/*****************************************************************************/

void doc_close(Document document){

    assert(document != NULL);

    fclose(document->doc_in);
    fclose(document->doc_out);
    free(document);
    document = NULL;
}
/*****************************************************************************/

void doc_put_word(Document document, char* word) {
    
    assert(word != NULL);
    assert(document != NULL);
    fprintf(document->doc_out, "%s", word);
}
/*****************************************************************************/