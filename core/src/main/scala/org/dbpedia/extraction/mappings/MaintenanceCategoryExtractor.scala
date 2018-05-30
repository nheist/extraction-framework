package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser._

import scala.language.reflectiveCalls

/**
 * Relies on various templates defining maintenance categories. A maintenance category is either a hidden or a tracking category.
 */
class MaintenanceCategoryExtractor(
  context : {
    def ontology : Ontology
    def language : Language
  }
)
extends PageNodeExtractor
{
    private val rdfTypeProperty = context.ontology.properties("rdf:type")

    private val parameterlessMaintenanceCategoryTemplates = Set("hidden category", "hiddencat", "tracking category", "trackingcat")
    private val maintenanceCategoryTemplates = Set("maintenance category", "maincat")

    override val datasets = Set(DBpediaDatasets.MaintenanceCategories)

    override def extract(page : PageNode, subjectUri : String): Seq[Quad] =
    {
        if (page.title.namespace == Namespace.Category)
        {
            if (hasMaintenanceTemplate(page)) {
                return Seq(new Quad(
                    context.language,
                    DBpediaDatasets.MaintenanceCategories,
                    subjectUri,
                    rdfTypeProperty,
                    "http://dbpedia.org/ontology/MaintenanceCategory",
                    null
                ))
            }
        }

        Seq.empty
    }

    private def hasMaintenanceTemplate(node : Node) : Boolean = node match
    {
        case maintenanceTemplateNode : TemplateNode if parameterlessMaintenanceCategoryTemplates.contains(maintenanceTemplateNode.title.decoded.toLowerCase()) || (maintenanceCategoryTemplates.contains(maintenanceTemplateNode.title.decoded.toLowerCase()) && (maintenanceTemplateNode.property("hidden").isDefined || maintenanceTemplateNode.property("tracking").isDefined)) => true
        case _ => node.children.map(hasMaintenanceTemplate).reduceOption(_ || _).getOrElse(false)
    }

}
