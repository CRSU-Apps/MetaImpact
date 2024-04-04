renderFooter <- function() {
  div(
    p(
      "THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT",
      "NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.",
      "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,",
      "WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE",
      "OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
    ),
    wellPanel(
      div(
        style = "display: inline;",
        img(src = 'funded-by-nihr-logo.png', width = "55%")
      ),
      div(
        style = "display: inline;",
        img(src = 'CRSU_logo.png', width = "40%")
      ),
      div(
        tags$strong("Funding and Support Acknowledgement:"),
        tags$p(
          "MetaImpact is part of the Complex Reviews Synthesis Unit (CRSU) suite of evidence synthesis apps.",
          "The development of these apps is currently funded (majority) and overseen by the Evidence Synthesis Group @ CRSU (NIHR153934).",
          "The CRSU Evidence Synthesis Group is one of the groups funded by the National Institute for Health and Care Research ",
          tags$a("(NIHR) Evidence Synthesis Programme.", href = "https://www.nihr.ac.uk/explore-nihr/funding-programmes/evidence-synthesis.htm"),
          "Further details of other funders and support, current and past, can be found ",
          tags$a("on our GitHub page", href = "https://github.com/CRSU-Apps/.github/wiki/Detailed-Funding-Statement", target = "_blank"),
          ". The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."
        ),
        tags$p(
          "More information about the UK NIHR Complex Reviews Synthesis Unit (CRSU) can be found ",
          tags$a("on our website.", href = "https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/", target = "_blank"),
        )
      )
    )
  )
}