package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.config.mappings.GenderExtractorConfig
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import util.matching.Regex
import org.dbpedia.extraction.ontology.datatypes.Datatype
import scala.language.reflectiveCalls

/**
 * Extracts redirects between categories using the "Category redirect" template.
 * (see also https://en.wikipedia.org/wiki/Template:Category_redirect)
 */
class CategoryRedirectExtractor(
    context: {
      def ontology: Ontology
      def language: Language
      def redirects: Redirects
    }
  ) extends PageNodeExtractor
{
  private val language = context.language

  private val wikiPageRedirectsProperty = context.ontology.properties("wikiPageRedirects")

  private val categoryPagePrefix = "Category:"

  override val datasets = Set(DBpediaDatasets.CategoryRedirects)

  override def extract(node : PageNode, subjectUri : String) : Seq[Quad] =
  {
    // if this node is a category
    if (node.title.namespace == Namespace.Category)
    {
      // get the page text
      val wikiText: String = node.toWikiText

      val regex = new Regex("\\{\\{category redirect\\|([^\\}]*)\\}\\}")
      val matchedRegex = regex.findAllIn(wikiText)

      if (matchedRegex.size >= 2)
      {
        var categoryIdentifier = matchedRegex.group(1)

        if (! categoryIdentifier.startsWith(categoryPagePrefix))
        {
          categoryIdentifier = categoryPagePrefix + categoryIdentifier
        }

        return Seq(new Quad(language, DBpediaDatasets.CategoryRedirects, subjectUri, wikiPageRedirectsProperty, language.resourceUri.append(categoryIdentifier), node.sourceIri, null))
      }
    }

    Seq.empty
  }

}
